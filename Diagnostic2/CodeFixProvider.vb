Imports Microsoft.CodeAnalysis.Rename

<ExportCodeFixProvider(DiagnosticAnalyzer.DiagnosticId, LanguageNames.VisualBasic)>
Friend Class CodeFixProvider
  Implements ICodeFixProvider

  Public Function GetFixableDiagnosticIds() As IEnumerable(Of String) Implements ICodeFixProvider.GetFixableDiagnosticIds
    Return {DiagnosticAnalyzer.DiagnosticId}
  End Function

  Public Async Function GetFixesAsync(document As Document, span As TextSpan, diagnostics As IEnumerable(Of Diagnostic), cancellationToken As CancellationToken) As Task(Of IEnumerable(Of CodeAction)) Implements ICodeFixProvider.GetFixesAsync
    Dim root = Await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(False)

    ' TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest

    Dim diagnosticSpan = diagnostics.First().Location.SourceSpan

    ' Find the type statement identified by the diagnostic.
    Dim declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType(Of TypeStatementSyntax)().First()

    ' Return a code action that will invoke the fix.
    Return {CodeAction.Create("Make uppercase", Function(c) MakeUppercaseAsync(document, declaration, c))}
  End Function

  Private Async Function MakeUppercaseAsync(document As Document, typeStmt As TypeStatementSyntax, cancellationToken As CancellationToken) As Task(Of Solution)
    ' Compute new uppercase name.
    Dim identifierToken = typeStmt.Identifier
    Dim newName = identifierToken.Text.ToUpperInvariant()

    ' Get the symbol representing the type to be renamed.
    Dim semanticModel = Await document.GetSemanticModelAsync(cancellationToken)
    Dim typeSymbol = semanticModel.GetDeclaredSymbol(typeStmt, cancellationToken)

    ' Produce a new solution that has all references to that type renamed, including the declaration.
    Dim originalSolution = document.Project.Solution
    Dim optionSet = originalSolution.Workspace.GetOptions()
    Dim newSolution = Await Renamer.RenameSymbolAsync(document.Project.Solution, typeSymbol, newName, optionSet, cancellationToken).ConfigureAwait(False)

    ' Return the new solution with the now-uppercase type name.
    Return newSolution
  End Function
End Class
