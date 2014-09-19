Option Strict On
Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Roslyn.StringFormatDiagnostics.VisualBasic.Exts
Imports Common
'Imports Roslyn.StringFormatDiagnostics
Imports AdamSpeight2008.StringFormatDiagnostic

<DiagnosticAnalyzer>
<ExportDiagnosticAnalyzer(Common.Common.DiagnosticId, LanguageNames.VisualBasic)>
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

  Private Shared _A As New Dictionary(Of String, Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic),CancellationToken))

  Shared Sub New()
    Common.Initialise 
  End Sub

  Sub New()
    If _A.Count <> 0 Then Exit Sub
    _A.Add("Num", AddressOf Check_Numeric_ToString)
    _A.Add("Date", AddressOf Check_DateTime_ToString)
    _A.Add("Enum", AddressOf Check_Enum_ToString)
    _A.Add("DateOff", AddressOf Check_DateTimeOffset_ToString)
    _A.Add("TS", AddressOf Check_TimeSpan_ToString)
  End Sub

  Public Sub AnalyzeNode(node As SyntaxNode,
                         semanticModel As SemanticModel,
                         addDiagnostic As Action(Of Diagnostic),
                         options As AnalyzerOptions,
                         cancellationToken As CancellationToken) Implements ISyntaxNodeAnalyzer(Of Microsoft.CodeAnalysis.VisualBasic.SyntaxKind).AnalyzeNode
    If node Is Nothing Then Exit Sub 
    If semanticModel Is Nothing Then Exit Sub
    If options Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
 
    Dim x = CType(node, MemberAccessExpressionSyntax)
    If x Is Nothing Then Exit Sub
    If x.OperatorToken.ValueText = "." Then
      Dim _MethodName = x.Name.ToString
      If _MethodName = "" Then Exit Sub
      Dim _CalledOnObjOfType = x.CalledOnType(semanticModel, cancellationToken)
      Dim _TypeName = If(_CalledOnObjOfType Is Nothing, "", _CalledOnObjOfType.ToFullyQualifiedName)
      Dim _TypeNameA() = _TypeName.Split("."c)
      Dim _InvokeExpr = TryCast(x.Parent, InvocationExpressionSyntax)
      If _InvokeExpr Is Nothing Then Exit Sub
      Dim Args = _InvokeExpr.ArgumentList

      Dim ArgTypes = Args.GetArgumentTypes(semanticModel, cancellationToken)
      Dim ArgTypeNames = Args.GetArgumentTypesNames(semanticModel, cancellationToken).ToArray
      ' Try to see if it is one the simple ones
      Dim possibles = From a In Common.Analysis
                      Where a(0) = _TypeName
                      Order By a.Count Descending

      If possibles.Any() = False Then Exit Sub
      For Each possible In possibles
        If ArgTypeNames.Begins(possible.Skip(4).ToArray) Then
          If possible(3) = "" Then
            DoValidation(x, semanticModel, addDiagnostic, cancellationToken)
              Exit Sub
          Else If _A.ContainsKey(possible(3)) Then
              Dim Validator = _A(possible(3))
              Validator(x, semanticModel, addDiagnostic, cancellationToken)
            Exit Sub
          End If
       
        End If
      Next
    End If
  End Sub

  Private Sub _Shared_Checker_(fn As Func(Of CancellationToken, String,Integer, IFormatProvider, OutputResult(Of String)),
                               node As MemberAccessExpressionSyntax,
                                 sm As SemanticModel,
                               addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    If fn Is Nothing Then Exit Sub
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit sub
    If addDiagnostic Is Nothing Then Exit Sub

    Dim p = CType(node.Parent, InvocationExpressionSyntax)


    Dim args = p.ArgumentList.Arguments
    Select Case args.Count
      Case 0 ' Error
      Case 1, 2
        Dim fs = args.First
        If TypeOf fs Is OmittedArgumentSyntax Then Exit Sub
        Dim TheFormatString = CType(fs, SimpleArgumentSyntax)
        If TheFormatString IsNot Nothing Then
          Dim ArgObjects = p.ArgumentList.GetArgumentAsObjects(sm, ct)
          Dim ifp = CType(If(args.Count = 1, Nothing, ArgObjects(1)), IFormatProvider)
          Select Case TheFormatString.Expression.VisualBasicKind
            Case SyntaxKind.StringLiteralExpression
              Dim ReportedIssues = fn(ct, Common.DeString(fs.ToString),1, ifp)
              For Each ReportedIssue In ReportedIssues.Errors
                If TypeOf ReportedIssue Is Interfaces.IReportIssueWithPositionAndLength Then
                  Dim ir = DirectCast(ReportedIssue, Interfaces.IReportIssueWithPositionAndLength)
                  Select Case ReportedIssue.Level
                    Case DiagnosticSeverity.Info
                      addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                    Case DiagnosticSeverity.Warning
                      addDiagnostic(AddWarning(fs, ir.Index, ir.Index + ir.Length, ReportedIssue))
                    Case DiagnosticSeverity.Error
                      addDiagnostic(AddError(fs, ir.Index, ir.Index + ir.Length, ReportedIssue))
                    Case DiagnosticSeverity.Hidden
                  End Select
                ElseIf TypeOf ReportedIssue Is Interfaces.IReportIssue Then
                  addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                End If
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
                Dim ReportedIssues = fn(ct, ConstValue.Value.ToString,1, ifp)
                For Each ReportedIssue In ReportedIssues.Errors
                  If TypeOf ReportedIssue Is Interfaces.IReportIssueWithPositionAndLength Then
                    Dim ir = DirectCast(ReportedIssue, Interfaces.IReportIssueWithPositionAndLength)
                    Select Case ReportedIssue.Level
                      Case DiagnosticSeverity.Info
                        addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                      Case DiagnosticSeverity.Warning
                        addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                      Case DiagnosticSeverity.Error
                        addDiagnostic(AddErrorAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                      Case DiagnosticSeverity.Hidden
                    End Select
                  ElseIf TypeOf ReportedIssue Is Interfaces.IReportIssue Then
                    addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                  End If
                Next
              Else
                ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                Dim ReportedIssues = fn(ct, ConstValue.Value.ToString, 1, ifp)
                For Each ReportedIssue In ReportedIssues.Errors
                  If TypeOf ReportedIssue Is Interfaces.IReportIssueWithPositionAndLength Then
                    Dim ir = DirectCast(ReportedIssue, Interfaces.IReportIssueWithPositionAndLength)
                    Select Case ReportedIssue.Level
                      Case DiagnosticSeverity.Info
                        addDiagnostic(AddInformation(TheValueOfTheVariable, ReportedIssue.Message))
                      Case DiagnosticSeverity.Warning
                        addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, ir.Index + 1, ir.Index + ir.Length + 1, ReportedIssue))
                      Case DiagnosticSeverity.Error
                        addDiagnostic(AddErrorAtSource(TheValueOfTheVariable, ir.Index + 1, ir.Index + ir.Length + 1, ReportedIssue))
                      Case DiagnosticSeverity.Hidden
                    End Select
                  ElseIf TypeOf ReportedIssue Is Interfaces.IReportIssue Then
                    addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                  End If
                Next
              End If
          End Select
        End If
    End Select
  End Sub


  Public Sub Check_TimeSpan_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_TimeSpan_ToString, node, sm, addDiagnostic, ct)
  End Sub

  Public Sub Check_Enum_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_Enum_ToString, node, sm, addDiagnostic, ct)
  End Sub

  Public Sub Check_DateTimeOffset_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_DateTimeOffset_ToString, node, sm, addDiagnostic, ct)
  End Sub

  Public Sub Check_DateTime_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_DateTime_ToString, node, sm, addDiagnostic, ct)
  End Sub

  Public Sub Check_Numeric_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_Numeric_ToString, node, sm, addDiagnostic, ct)
  End Sub

  Public Shared Sub DoValidation(node As MemberAccessExpressionSyntax,
                          sm As SemanticModel,
                          addDiagnostic As Action(Of Diagnostic),
                          ct As CancellationToken,
                          Optional formatIsFirst As Boolean = True)
      If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub

    Dim p = CType(node.Parent, InvocationExpressionSyntax)
    Dim args = p.ArgumentList.Arguments
    Select Case args.Count
      Case 0 ' Error
      Case Else
        Dim fs = If(formatIsFirst, args.First, args(1))
        If TypeOf fs Is OmittedArgumentSyntax Then Exit Sub
        Dim TheFormatString = CType(fs, SimpleArgumentSyntax)
        If TheFormatString IsNot Nothing Then
          Dim ArgObjects = p.ArgumentList.GetArgumentAsObjects(sm, ct)
          Select Case TheFormatString.Expression.VisualBasicKind
            Case SyntaxKind.StringLiteralExpression
              Dim ReportedIssues = AnalyseFormatString(ct, fs.ToString, args.Count - If(formatIsFirst, 1, 2), ArgObjects.Skip(If(formatIsFirst, 1, 2)).ToArray, CType(If(formatIsFirst, Nothing, ArgObjects(0)), IFormatProvider))
              For Each ReportedIssue In ReportedIssues.Errors
                If TypeOf ReportedIssue Is Interfaces.IReportIssueWithPositionAndLength Then
                  Dim ir = DirectCast(ReportedIssue, Interfaces.IReportIssueWithPositionAndLength)
                  Select Case ReportedIssue.Level
                    Case DiagnosticSeverity.Info
                      addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                    Case DiagnosticSeverity.Warning
                      addDiagnostic(AddWarning(fs, ir.Index, ir.Index + ir.Length, ReportedIssue))
                    Case DiagnosticSeverity.Error
                      addDiagnostic(AddError(fs, ir.Index, ir.Index + ir.Length, ReportedIssue))
                    Case DiagnosticSeverity.Hidden
                  End Select
                ElseIf TypeOf ReportedIssue Is Interfaces.IReportIssue Then
                  addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                End If
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
                Dim ReportedIssues = AnalyseFormatString(ct, ConstValue.Value.ToString, args.Count - If(formatIsFirst, 1, 2), ArgObjects.Skip(If(formatIsFirst, 1, 2)).ToArray, CType(If(formatIsFirst, Nothing, ArgObjects(0)), IFormatProvider))
                For Each ReportedIssue In ReportedIssues.Errors
                  If TypeOf ReportedIssue Is Interfaces.IReportIssueWithPositionAndLength Then
                    Dim ir = DirectCast(ReportedIssue, Interfaces.IReportIssueWithPositionAndLength)
                    Select Case ReportedIssue.Level
                      Case DiagnosticSeverity.Info
                        addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                      Case DiagnosticSeverity.Warning
                        addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                      Case DiagnosticSeverity.Error
                        addDiagnostic(AddErrorAtSource(fs, 0,  fs.Span.Length, ReportedIssue))
                      Case DiagnosticSeverity.Hidden
                    End Select
                  ElseIf TypeOf ReportedIssue Is Interfaces.IReportIssue Then
                    addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                  End If
                Next
              Else
                ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                Dim ReportedIssues = AnalyseFormatString(ct, ConstValue.Value.ToString, args.Count - If(formatIsFirst, 1, 2), ArgObjects.Skip(If(formatIsFirst, 1, 2)).ToArray, CType(If(formatIsFirst, Nothing, ArgObjects(0)), IFormatProvider))
                For Each ReportedIssue In ReportedIssues.Errors
                  If TypeOf ReportedIssue Is Interfaces.IReportIssueWithPositionAndLength Then
                    Dim ir = DirectCast(ReportedIssue, Interfaces.IReportIssueWithPositionAndLength)
                    Select Case ReportedIssue.Level
                      Case DiagnosticSeverity.Info
                        addDiagnostic(AddInformation(TheValueOfTheVariable, ReportedIssue.Message))
                      Case DiagnosticSeverity.Warning
                        addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, ir.Index + 1, ir.Index + ir.Length + 1, ReportedIssue))
                      Case DiagnosticSeverity.Error
                        addDiagnostic(AddErrorAtSource(TheValueOfTheVariable, ir.Index+1 , ir.Index+ir.Length+1, ReportedIssue))
                      Case DiagnosticSeverity.Hidden
                    End Select
                  ElseIf TypeOf ReportedIssue Is Interfaces.IReportIssue Then
                    addDiagnostic(AddInformation(TheValueOfTheVariable, ReportedIssue.Message))
                  End If
                Next
              End If
          End Select
        End If
    End Select

  End Sub

End Class

