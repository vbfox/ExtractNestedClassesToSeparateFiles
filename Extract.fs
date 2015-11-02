open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.Formatting
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

open FluentRoslynLite

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
    |> (fun f -> f.WithAdditionalAnnotations(Formatter.Annotation))

let splitNestedThings (root:SyntaxNode) (document:Document) (projectId:ProjectId) (solution:Solution) = async {
    let classes = root.DescendantNodesAndSelf() |> ofType<_,ClassDeclarationSyntax> |> List.ofSeq
    let nsNodes = root.DescendantNodesAndSelf() |> ofType<_,NamespaceDeclarationSyntax> |> List.ofSeq
    if classes.IsEmpty || nsNodes.IsEmpty then
        return solution
    else
        let ns = (nsNodes.Head.Name :?> IdentifierNameSyntax).Identifier.Text
        let usings = root.DescendantNodesAndSelf() |> ofType<_,UsingDirectiveSyntax> |> List.ofSeq
        let rootClass = classes.Head
        let nested = rootClass.ChildNodes() |> ofType<_,BaseTypeDeclarationSyntax> |> List.ofSeq

        let expectedFileNames = nested |> List.map (fun x -> x, sprintf "%s+%s.cs" rootClass.Identifier.Text x.Identifier.Text)
        
        let fileName = Path.GetFileName document.FilePath
        let mutable solution = solution
        let mutable toRemove = []
        for (nestedType, expectedFileName) in expectedFileNames do
            if fileName <> expectedFileName then
                printfn "Extracting %s from %s" expectedFileName fileName
                let newFile = makeExtractedFile ns usings rootClass nestedType
                let currentProject = solution.GetProject(projectId)
                let doc = currentProject.AddDocument(expectedFileName, newFile)
                let! formattedDoc = !! Formatter.FormatAsync(doc)

                solution <- formattedDoc.Project.Solution
                toRemove <- nestedType :: toRemove

        let nodesToRemove = toRemove |> Seq.map(fun x -> (x :> SyntaxNode))
        let modifiedRoot = root.RemoveNodes(nodesToRemove, SyntaxRemoveOptions.AddElasticMarker)

        return solution.WithDocumentSyntaxRoot(document.Id, modifiedRoot)
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
        let! solution = !! ws.OpenSolutionAsync(@"E:\Code\pinvoke\src\PInvoke.sln")
        printfn "Done."

        let! newSolution = splitSolutionNestedThings solution
        if not (ws.TryApplyChanges(newSolution)) then
            failwith "Can't change"
        ()
    } |> Async.RunSynchronously
    
    printfn "Done."
    0
