open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Simplification
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.Diagnostics
open System.Collections.Immutable
open System.IO

module FluentRoslynLite =
    let (!!) t = Async.AwaitTask t
    let emptyFile = SyntaxFactory.CompilationUnit()
    let identifier (identifierName : string) = SyntaxFactory.IdentifierName(identifierName)
    let inline addMembers members (input:^T) =
        (^T : (member AddMembers : MemberDeclarationSyntax array -> ^T) (input, members |> Seq.toArray))
    let inline addMember member' (input:^T) =
        (^T : (member AddMembers : MemberDeclarationSyntax array -> ^T) (input, [|member'|]))
    let inline addModifiers syntaxKinds (input:^T) =
        let tokens = syntaxKinds |> Seq.map (fun k -> SyntaxFactory.Token(k)) |> Seq.toArray
        (^T : (member AddModifiers : SyntaxToken array -> ^T) (input, tokens))
    let class' (name:string) = SyntaxFactory.ClassDeclaration(name)
    let namespace' (name:string) = SyntaxFactory.NamespaceDeclaration(identifier name)
    let addAnnotations annotations node = node.WithAdditionalAnnotations(annotations |> Seq.toArray)
    let addTriviaAfter (trivia : SyntaxTrivia seq) (node : #SyntaxNode) =
        let newTrivia = Seq.concat [node.GetTrailingTrivia() :> SyntaxTrivia seq; trivia] |> Seq.toArray
        node.WithTrailingTrivia(newTrivia)
    let addTriviaSAfter (trivia : string) (node : #SyntaxNode) =
        let trivia = SyntaxFactory.ParseTrailingTrivia(trivia)
        addTriviaAfter trivia node
    let addTriviaBefore (trivia : SyntaxTrivia seq) (node : #SyntaxNode) =
        let newTrivia = Seq.concat [trivia; node.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        node.WithLeadingTrivia(newTrivia)
    let addTriviaSBefore (trivia : string) (node : #SyntaxNode) =
        let trivia = SyntaxFactory.ParseLeadingTrivia(trivia)
        addTriviaBefore trivia node
    let toImmutableList seq = ImmutableList.Empty.AddRange(seq)

open FluentRoslynLite

let header = """// Copyright (c) to owners found in https://github.com/AArnott/pinvoke/blob/master/COPYRIGHT.md. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.

"""

let ofType<'a, 'T> (seq : 'a seq) = 
    seq
        |> Seq.choose (fun x -> 
            match box x with
            | :? 'T as x -> Some(x)
            | _ -> None
        )

let getCuteName (decl: BaseTypeDeclarationSyntax) = 
    match decl with
    | :? ClassDeclarationSyntax -> "class"
    | :? StructDeclarationSyntax -> "struct"
    | :? EnumDeclarationSyntax -> "enum"
    | _ -> failwith "Unexpected type"

let getDoc (decl:BaseTypeDeclarationSyntax) = 
    let text =
        sprintf
            "\r\n/// <content>\r\n/// Contains the <see cref=\"%s\"/> nested %s.\r\n/// </content>\r\n"
            decl.Identifier.Text
            (getCuteName decl)

    SyntaxFactory.ParseLeadingTrivia(text)

let makeExtractedFile ns usings (rootClass:ClassDeclarationSyntax) nestedType =
    emptyFile
    |> addMembers
        [
            namespace' ns
            |> (fun ns -> ns.WithUsings(SyntaxFactory.List(usings)))
            |> addMembers
                [
                    class' rootClass.Identifier.Text
                    |> addModifiers [ SyntaxKind.PublicKeyword; SyntaxKind.PartialKeyword ]
                    |> addMember nestedType
                    |> (fun c -> c.WithLeadingTrivia(getDoc nestedType))
                ]
        ]
    |> addAnnotations [ Formatter.Annotation ]
    |> addTriviaSAfter "\r\n\r\n"
    |> addTriviaSBefore header

let formatDoc doc = async { return! !! Formatter.FormatAsync(doc) }
let reduceDoc doc = async { return! !! Simplifier.ReduceAsync(doc) }
let cleanup doc = async {
    let! formatted = formatDoc doc
    return! reduceDoc formatted
}

let splitNestedThings (root:SyntaxNode) (document:Document) (projectId:ProjectId) (solution:Solution) = async {
    let classes = root.DescendantNodesAndSelf() |> ofType<_,ClassDeclarationSyntax> |> List.ofSeq
    let nsNodes = root.DescendantNodesAndSelf() |> ofType<_,NamespaceDeclarationSyntax> |> List.ofSeq
    if classes.IsEmpty || nsNodes.IsEmpty then
        return solution
    else
        let ns = (nsNodes.Head.Name :?> IdentifierNameSyntax).Identifier.Text
        let usings =
            root.DescendantNodesAndSelf()
            |> ofType<_,UsingDirectiveSyntax>
        let usingsForNewFiles = usings |> Seq.map (addAnnotations [Simplifier.Annotation]) |> List.ofSeq
        let rootClass = classes.Head
        let nested = rootClass.ChildNodes() |> ofType<_,BaseTypeDeclarationSyntax> |> List.ofSeq

        let expectedFileNames = nested |> List.map (fun x -> x, sprintf "%s+%s.cs" rootClass.Identifier.Text x.Identifier.Text)
        
        let fileName = Path.GetFileName document.FilePath
        let mutable solution = solution
        let mutable toRemove = []
        for (nestedType, expectedFileName) in expectedFileNames do
            if fileName <> expectedFileName then
                printfn "Extracting %s from %s" expectedFileName fileName
                let newFile = makeExtractedFile ns usingsForNewFiles rootClass nestedType
                let currentProject = solution.GetProject(projectId)
                let doc = currentProject.AddDocument(expectedFileName, newFile)
                let! cleanDoc = cleanup doc
                solution <- cleanDoc.Project.Solution
                toRemove <- nestedType :: toRemove

        let nodesToRemove = toRemove |> Seq.map(fun x -> (x :> SyntaxNode))
        let modifiedRoot = root.RemoveNodes(nodesToRemove, SyntaxRemoveOptions.AddElasticMarker)
        
        let replacement = fun (n : UsingDirectiveSyntax) (_ :UsingDirectiveSyntax) -> addAnnotations [Simplifier.Annotation] n :> SyntaxNode
        let rootWithAnnotatedUsings = modifiedRoot.ReplaceNodes(usings |> toImmutableList,  new System.Func<_, _, _>(replacement)) 

        solution <- solution.WithDocumentSyntaxRoot(document.Id, rootWithAnnotatedUsings)

        let! cleanDoc = cleanup (solution.GetDocument(document.Id))
        solution <- cleanDoc.Project.Solution

        return solution
}

let splitSolutionNestedThings (solution:Solution) = async {
    let mutable currentSolution = solution

    for project in solution.Projects do
        for doc in project.Documents do
            let currentDoc = currentSolution.GetDocument(doc.Id)
            let! syntaxTree = !! currentDoc.GetSyntaxTreeAsync()
            let! root = !! syntaxTree.GetRootAsync()
            let! modifiedSolution = splitNestedThings root currentDoc project.Id currentSolution

            currentSolution <- modifiedSolution

    return currentSolution
}

[<EntryPoint>]
let main _ = 
    use ws = MSBuildWorkspace.Create()

    async {
        printf "Loading solution... "
        let! solution = !! ws.OpenSolutionAsync(@"C:\Code\pinvoke\src\PInvoke.sln")
        printfn "Done."

        let! newSolution = splitSolutionNestedThings solution
        if not (ws.TryApplyChanges(newSolution)) then
            failwith "Can't change"
        ()
    } |> Async.RunSynchronously
    
    printfn "Done."
    0