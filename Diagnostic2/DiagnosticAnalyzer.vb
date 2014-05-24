Option Strict On
Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Roslyn.StringFormatDiagnostics
Imports Roslyn.StringFormatDiagnostics.Common
<DiagnosticAnalyzer>
<ExportDiagnosticAnalyzer(DiagnosticId, LanguageNames.VisualBasic)>
Public Class DiagnosticAnalyzer
  Implements ISyntaxNodeAnalyzer(Of Microsoft.CodeAnalysis.VisualBasic.SyntaxKind)
  Public ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor) Implements IDiagnosticAnalyzer.SupportedDiagnostics
    Get
      Return ImmutableArray.Create(Rule1, Rule2)
    End Get
  End Property
  Private ReadOnly Property SyntaxKindsOfInterest As ImmutableArray(Of SyntaxKind) Implements ISyntaxNodeAnalyzer(Of SyntaxKind).SyntaxKindsOfInterest
    Get
      Return ImmutableArray.Create(SyntaxKind.SimpleMemberAccessExpression)
    End Get
  End Property
  Public Sub AnalyzeNode(node As SyntaxNode, semanticModel As SemanticModel, addDiagnostic As Action(Of Diagnostic), cancellationToken As CancellationToken) Implements ISyntaxNodeAnalyzer(Of SyntaxKind).AnalyzeNode
    Dim x = CType(node, MemberAccessExpressionSyntax)
    If x Is Nothing Then Exit Sub
    Select Case x.ToString.ToLower
      Case "string.format", "console.write", "console.writeline"
        Dim p = CType(x.Parent, InvocationExpressionSyntax)
        Dim args = p.ArgumentList.Arguments
        Select Case args.Count
          Case 0 ' Error
          Case Else
            Dim fs = args.First
            If TypeOf fs Is OmittedArgumentSyntax Then Exit Sub
            Dim TheFormatString = CType(fs, SimpleArgumentSyntax)
            If TheFormatString IsNot Nothing Then
              Select Case TheFormatString.Expression.VisualBasicKind
                Case SyntaxKind.StringLiteralExpression
                  Dim ReportedIssues = AnalyseFormatString(cancellationToken, fs.ToString, args.Count - 1)
                  For Each ReportedIssue In ReportedIssues
                    Select Case ReportedIssue
                      Case cex As ArgIndexOutOfRange : addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                      Case cex As UnexpectedChar : addDiagnostic(AddWarning(fs, cex.Start, cex.Start + 1, ReportedIssue))
                      Case cex As UnexpectedlyReachedEndOfText : addDiagnostic(AddWarning(fs, 0, fs.Span.Length, ReportedIssue))
                      Case cex As ArgIndexHasExceedLimit : addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                      Case ___ As Internal_IssueReport : addDiagnostic(AddWarning(node, 0, fs.Span.Length, ReportedIssue))
                      Case ___ As ContainsNoArgs : addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                    End Select
                  Next
                Case SyntaxKind.IdentifierName
                  Dim ThisIdentifier = CType(TheFormatString.Expression, IdentifierNameSyntax)
                  If ThisIdentifier Is Nothing Then Exit Sub
                  Dim ConstValue = semanticModel.GetConstantValue(ThisIdentifier, cancellationToken)
                  If ConstValue.HasValue = False Then Exit Sub
                  Dim FoundSymbol = semanticModel.LookupSymbols(TheFormatString.Expression.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
                  Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, VariableDeclaratorSyntax)
                  If VariableDeclarationSite Is Nothing Then Exit Sub
                  Dim TheValueOfTheVariable = VariableDeclarationSite.Initializer.Value
                  'Debugger.Break()
                  If FoundSymbol.IsExtern Then
                    ' Use usage site for location of Warings, ignore the yield ranges and use the span of ThisIdentifier.
                    Dim ReportedIssues = AnalyseFormatString(cancellationToken, ConstValue.Value.ToString, args.Count - 1)
                    For Each ReportedIssue In ReportedIssues
                      Select Case ReportedIssue
                        Case cex As ArgIndexOutOfRange : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                        Case cex As UnexpectedChar : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                        Case cex As UnexpectedlyReachedEndOfText : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                        Case cex As ArgIndexHasExceedLimit : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                        Case ___ As Internal_IssueReport : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                        Case ___ As ContainsNoArgs : addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                      End Select
                    Next
                  Else
                    ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                    Dim ReportedIssues = AnalyseFormatString(cancellationToken, ConstValue.Value.ToString, args.Count - 1)
                    For Each ReportedIssue In ReportedIssues
                      Select Case ReportedIssue
                        Case cex As ArgIndexOutOfRange : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                        Case cex As UnexpectedChar : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, cex.Start + 2, ReportedIssue))
                        Case cex As UnexpectedlyReachedEndOfText : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                        Case cex As ArgIndexHasExceedLimit : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                        Case ___ As Internal_IssueReport : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                        Case ___ As ContainsNoArgs : addDiagnostic(AddInformation(TheValueOfTheVariable, "Contains no args! Are you sure this Is correct?"))
                      End Select
                    Next
                  End If
              End Select
            End If
        End Select
    End Select
  End Sub
End Class