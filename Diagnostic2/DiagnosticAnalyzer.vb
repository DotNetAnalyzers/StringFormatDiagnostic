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
    If x.OperatorToken.ValueText = "." Then
      Dim mn = x.Name.ToString
      If mn = "" Then Exit Sub
      Dim ct = x.CalledOnType(semanticModel, cancellationToken)
      Dim tn = If(ct Is Nothing, "", ct.Name)
      Dim invokation = TryCast(x.Parent, InvocationExpressionSyntax)
      If invokation Is Nothing Then Exit Sub 
      Dim Args = invokation.ArgumentList
      Dim ArgTypes = Args.GetArgumentTypes(semanticModel, cancellationToken)
      Dim ArgTypeNames = Args.GetArgumentTypesNames(semanticModel, cancellationToken)
      Select Case tn
        Case "Console"
          Select Case mn
            Case "Write", "WriteLine" : DoValidation(x, semanticModel, addDiagnostic, cancellationToken)
          End Select
        Case "String"
          Select Case mn
            Case "Format" : DoValidation(x, semanticModel, addDiagnostic, cancellationToken)
          End Select
        Case "StringBuilder"
          Select Case mn
            Case "AppendFormat"
              Select Case Args.Arguments.Count
                Case 0, 1 : Exit Sub
                Case 2, 3, 4 When ArgTypeNames(0) = "String" : DoValidation(x, semanticModel, addDiagnostic, cancellationToken)

                Case Else
                  If ArgTypeNames(0) = "String" Then DoValidation(x, semanticModel, addDiagnostic, cancellationToken)
              End Select
          End Select
        Case Else
          Select Case mn
            Case "ToString"
              Select Case Args.Arguments.Count
                Case 1
                  Select Case ArgTypeNames(0)
                    Case "String"
                      Dim ff = Microsoft.CodeAnalysi
                    Case "IFormatProvider"
                  End Select
                Case 2
                  If ArgTypeNames(0) = "String" AndAlso ArgTypes(1).Interfaces.Any(Function(i) i.Name = "IFormatProvider") Then

                  End If
                Case Else
                  Exit Sub
              End Select
          End Select
      End Select
    End If
  End Sub


  Public Sub DoValidation(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    Dim p = CType(node.Parent, InvocationExpressionSyntax)
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
              Dim ReportedIssues = AnalyseFormatString(ct, fs.ToString, args.Count - 1)
              For Each ReportedIssue In ReportedIssues
                Select Case ReportedIssue
                  Case cex As ArgIndexOutOfRange : addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                  Case cex As UnexpectedChar : addDiagnostic(AddWarning(fs, cex.Start, cex.Start + 1, ReportedIssue))
                  Case cex As UnexpectedlyReachedEndOfText : addDiagnostic(AddWarning(fs, 0, fs.Span.Length, ReportedIssue))
                  Case cex As ArgIndexHasExceedLimit : addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                  Case ___ As ContainsNoArgs : addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                  Case ___ As Internal_IssueReport : addDiagnostic(AddWarning(node, 0, fs.Span.Length, ReportedIssue))
                End Select
              Next
            Case SyntaxKind.IdentifierName
              Dim ThisIdentifier = CType(TheFormatString.Expression, IdentifierNameSyntax)
              If ThisIdentifier Is Nothing Then Exit Sub
              Dim ConstValue = sm.GetConstantValue(ThisIdentifier, ct)
              If ConstValue.HasValue = False Then Exit Sub
              Dim FoundSymbol = sm.LookupSymbols(TheFormatString.Expression.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
              Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, VariableDeclaratorSyntax)
              If VariableDeclarationSite Is Nothing Then Exit Sub
              Dim TheValueOfTheVariable = VariableDeclarationSite.Initializer.Value
              'Debugger.Break()
              If FoundSymbol.IsExtern Then
                ' Use usage site for location of Warings, ignore the yield ranges and use the span of ThisIdentifier.
                Dim ReportedIssues = AnalyseFormatString(ct, ConstValue.Value.ToString, args.Count - 1)
                For Each ReportedIssue In ReportedIssues
                  Select Case ReportedIssue
                    Case cex As ArgIndexOutOfRange : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case cex As UnexpectedChar : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case cex As UnexpectedlyReachedEndOfText : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case cex As ArgIndexHasExceedLimit : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case ___ As ContainsNoArgs : addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                    Case ___ As Internal_IssueReport : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                  End Select
                Next
              Else
                ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                Dim ReportedIssues = AnalyseFormatString(ct, ConstValue.Value.ToString, args.Count - 1)
                For Each ReportedIssue In ReportedIssues
                  Select Case ReportedIssue
                    Case cex As ArgIndexOutOfRange : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                    Case cex As UnexpectedChar : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, cex.Start + 2, ReportedIssue))
                    Case cex As UnexpectedlyReachedEndOfText : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                    Case cex As ArgIndexHasExceedLimit : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                    Case ___ As ContainsNoArgs : addDiagnostic(AddInformation(TheValueOfTheVariable, "Contains no args! Are you sure this Is correct?"))
                    Case ___ As Internal_IssueReport : addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                  End Select
                Next
              End If
          End Select
        End If
    End Select

  End Sub

End Class

Public Module Exts

  <Runtime.CompilerServices.Extension>
  Public Function ArgumentType(arg As ArgumentSyntax, sm As SemanticModel, ct As CancellationToken) As ITypeSymbol
    Return sm.GetTypeInfo(CType(arg, Syntax.SimpleArgumentSyntax).Expression, ct).Type

  End Function
  <Runtime.CompilerServices.Extension>
  Public Function GetArgumentTypes(args As ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of ITypeSymbol)
    Return args.Arguments.Select(Function(arg) arg.ArgumentType(sm, ct))
  End Function
  <Runtime.CompilerServices.Extension>
  Public Function GetArgumentTypesNames(args As ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of String)
    Return args.GetArgumentTypes(sm, ct).Select(Function(tsym) tsym.Name)
  End Function
  <Runtime.CompilerServices.Extension>
  Public Function CalledOnType(n As MemberAccessExpressionSyntax, sm As SemanticModel, ct As CancellationToken) As INamedTypeSymbol
    Dim s = sm.GetSymbolInfo(n, ct).Symbol
    If s Is Nothing Then Return Nothing
    Dim t = s.ContainingType
    Return t
  End Function
End Module