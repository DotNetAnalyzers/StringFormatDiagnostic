﻿Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis.Rename

<ExportCodeFixProvider("Analyzer1CodeFixProvider", LanguageNames.VisualBasic), [Shared]>
Public Class Analyzer1CodeFixProvider
  Inherits CodeFixProvider

  Public NotOverridable Overrides Function GetFixableDiagnosticIds() As ImmutableArray(Of String)
    Return ImmutableArray.Create(Analyzer1Analyzer.DiagnosticId)
  End Function

  Public NotOverridable Overrides Function GetFixAllProvider() As FixAllProvider
    Return WellKnownFixAllProviders.BatchFixer
  End Function

  Public NotOverridable Overrides Async Function ComputeFixesAsync(context As CodeFixContext) As Task
    Dim root = Await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(False)

    ' TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest

    Dim diagnostic = context.Diagnostics.First()
    Dim diagnosticSpan = diagnostic.Location.SourceSpan

    ' Find the type statement identified by the diagnostic.
    Dim declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType(Of TypeStatementSyntax)().First()

    ' Register a code action that will invoke the fix.
    context.RegisterFix(
        CodeAction.Create("Make uppercase", Function(c) MakeUppercaseAsync(context.Document, declaration, c)),
        diagnostic)
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
    Dim optionSet = originalSolution.Workspace.Options
    Dim newSolution = Await Renamer.RenameSymbolAsync(document.Project.Solution, typeSymbol, newName, optionSet, cancellationToken).ConfigureAwait(False)

    ' Return the new solution with the now-uppercase type name.
    Return newSolution
  End Function
End Class