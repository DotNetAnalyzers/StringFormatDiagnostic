Option Strict On
Imports System.Collections.Immutable
'Imports Roslyn.StringFormatDiagnostics
Imports AdamSpeight2008.StringFormatDiagnostic
Imports AdamSpeight2008.StringFormatDiagnostic.Analysers
Imports AdamSpeight2008.StringFormatDiagnostic.Common
Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces
Imports AdamSpeight2008.StringFormatDiagnostic.VisualBasic
Imports AdamSpeight2008.StringFormatDiagnostic.Common.Common_StringFormat
Imports Microsoft.CodeAnalysis.Diagnostics

<DiagnosticAnalyzer>
<ExportDiagnosticAnalyzer(DiagnosticId, "Visual Basic")>
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


  Private Shared _DictOfAnalysers As New Concurrent.ConcurrentDictionary(Of String,
    Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object)))

  Shared Sub New()
    Initialise()
  End Sub

  Sub New()
    If _DictOfAnalysers.Count <> 0 Then Exit Sub
    _DictOfAnalysers = New Concurrent.ConcurrentDictionary(Of String, Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object))) From
     {{"SF", AddressOf Check_FormatString}, {"Num", AddressOf Check_Numeric_ToString}, {"Date", AddressOf Check_DateTime_ToString},
      {"Enum", AddressOf Check_Enum_ToString}, {"DateOff", AddressOf Check_DateTimeOffset_ToString}, {"TS", AddressOf Check_TimeSpan_ToString}}
  End Sub
  Public Sub AnalyzeNode(node As SyntaxNode, semanticModel As SemanticModel, addDiagnostic As Action(Of Diagnostic), cancellationToken As CancellationToken) Implements ISyntaxNodeAnalyzer(Of SyntaxKind).AnalyzeNode
     If AnyIsNull(Of Object)({node, semanticModel,  addDiagnostic}) Then Exit Sub
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
      Dim ArgObjs = Args.GetArgumentAsObjects(semanticModel, cancellationToken)
      Dim ArgTypeNames = Args.GetArgumentTypesNames(semanticModel, cancellationToken).ToArray
      ' Try to see if it is one the simple ones
      Dim possibles = From a In Analysis.AsParallel.AsOrdered Where a.TypeName = _TypeName Order By a.ParamTypes.Count Descending
      possibles.IfAnyThenDo(Sub(ps)
                              ps.AsParallel.
                                  AsOrdered.
                                  Where(Function(p) ArgTypeNames.BeginsWith(p.ParamTypes) AndAlso _DictOfAnalysers.ContainsKey(p.Analyser)).
                                  Select(Function(p) New With {.a = _DictOfAnalysers(p.Analyser), .p = p}).
                                  IfAnyThenDo(Sub(q) q(0).a.Invoke(x, semanticModel, addDiagnostic, cancellationToken, q(0).p.FIndex, ArgObjs))
                            End Sub)
    End If
  End Sub

  Private Sub _Shared_Checker_(fn As Func(Of CancellationToken, String, Integer, IFormatProvider, IEnumerable(Of Object), OutputResult(Of String)),
                                 node As MemberAccessExpressionSyntax,
                                   sm As SemanticModel,
                                 diag As Action(Of Diagnostic), ct As CancellationToken, FSIndex As Integer,
                                        Args As IEnumerable(Of Object))
    If AnyIsNull(Of Object)({fn, node, sm, diag}) Then Exit Sub
    If FSIndex < 0 Then Exit Sub
    Dim p = CType(node.Parent, InvocationExpressionSyntax)
    Select Case Args.Count
      Case 0 ' Error
      Case Is > 0
        Dim fs = p.ArgumentList.Arguments(FSIndex) 'args.First
        If TypeOf fs Is OmittedArgumentSyntax Then Exit Sub
        Dim TheFormatString = CType(fs, SimpleArgumentSyntax)
        If TheFormatString IsNot Nothing Then
          'Dim ArgObjects = p.ArgumentList.GetArgumentAsObjects(sm, ct)
          Dim fp = Args.TakeWhile(Function(a) TypeOf a IsNot IFormatProvider)
          Dim sk = (Args.Count - fp.Count) - 1
          Dim ifp = CType(If(sk < 0, Nothing, Args(sk + 1)), IFormatProvider) ' CType(If(args.Count = 1, Nothing, ArgObjects(1)), IFormatProvider)
          Args = If(sk < 0, Args.Skip(FSIndex + 1), Args.Skip(sk))
          Select Case TheFormatString.Expression.VisualBasicKind
            Case SyntaxKind.StringLiteralExpression
              Dim ReportedIssues = fn(ct, DeString(fs.ToString), 1, ifp, Args)
              For Each ReportedIssue In ReportedIssues.Errors
                If TypeOf ReportedIssue Is Interfaces.IReportIssueWithPositionAndLength Then
                  Dim ir = DirectCast(ReportedIssue, Interfaces.IReportIssueWithPositionAndLength)
                  Select Case ir.Level
                    Case DiagnosticSeverity.Info : diag(AddInformation(fs, ir.Message))
                    Case DiagnosticSeverity.Warning : diag(AddWarning(fs, ir.Index, ir.Index + ir.Length, ir))
                    Case DiagnosticSeverity.Error : diag(AddError(fs, ir.Index, ir.Index + ir.Length, ir))
                    'Case DiagnosticSeverity.Hidden
                  End Select
                ElseIf TypeOf ReportedIssue Is Interfaces.IReportIssue Then
                  diag(AddInformation(fs, DirectCast( ReportedIssue,IReportIssue).Message))
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
                Dim ReportedIssues = fn(ct, ConstValue.Value.ToString, 0, ifp, Args)
                For Each ReportedIssue In ReportedIssues.Errors
                  If TypeOf ReportedIssue Is Interfaces.IReportIssueWithPositionAndLength Then
                    Dim ir = DirectCast(ReportedIssue, Interfaces.IReportIssueWithPositionAndLength)
                    Select Case ir.Level
                      Case DiagnosticSeverity.Info : diag(AddInformation(fs, ir.Message))
                      Case DiagnosticSeverity.Warning : diag(AddWarningAtSource(fs, 0, fs.Span.Length, ir))
                      Case DiagnosticSeverity.Error : diag(AddErrorAtSource(fs, 0, fs.Span.Length, ir))
                      'Case DiagnosticSeverity.Hidden
                    End Select
                  ElseIf TypeOf ReportedIssue Is Interfaces.IReportIssue Then
                    diag(AddInformation(fs, DirectCast( ReportedIssue,IReportIssue).Message))
                  End If
                Next
              Else
                ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                Dim ReportedIssues = fn(ct, ConstValue.Value.ToString, 0, ifp, Args)
                For Each ReportedIssue In ReportedIssues.Errors
                  If TypeOf ReportedIssue Is Interfaces.IReportIssueWithPositionAndLength Then
                    Dim ir = DirectCast(ReportedIssue, Interfaces.IReportIssueWithPositionAndLength)
                    Select Case ir.Level
                      Case DiagnosticSeverity.Info : diag(AddInformation(TheValueOfTheVariable, ir.Message))
                      Case DiagnosticSeverity.Warning : diag(AddWarningAtSource(TheValueOfTheVariable, ir.Index + 1, ir.Index + ir.Length + 1, ir))
                      Case DiagnosticSeverity.Error : diag(AddErrorAtSource(TheValueOfTheVariable, ir.Index + 1, ir.Index + ir.Length + 1, ir))
                        'Case DiagnosticSeverity.Hidden
                    End Select
                  ElseIf TypeOf ReportedIssue Is Interfaces.IReportIssue Then
                    diag(AddInformation(fs, DirectCast( ReportedIssue,IReportIssue).Message))
                  End If
                Next
              End If
          End Select
        End If
    End Select
  End Sub

  Public Sub Check_FormatString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    _Shared_Checker_(AddressOf Common_StringFormat.AnalyseFormatString, node, sm, addDiagnostic, ct, fsi, ArgObjs)
  End Sub

  Public Sub Check_TimeSpan_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_TimeSpan_ToString, node, sm, addDiagnostic, ct, fsi, ArgObjs)
  End Sub

  Public Sub Check_Enum_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_Enum_ToString, node, sm, addDiagnostic, ct, fsi, ArgObjs)
  End Sub

  Public Sub Check_DateTimeOffset_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_DateTimeOffset_ToString, node, sm, addDiagnostic, ct, fsi, ArgObjs)
  End Sub

  Public Sub Check_DateTime_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_DateTime_ToString, node, sm, addDiagnostic, ct, fsi, ArgObjs)
  End Sub

  Public Sub Check_Numeric_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    If sm Is Nothing Then Exit Sub
    If addDiagnostic Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_Numeric_ToString, node, sm, addDiagnostic, ct, fsi, ArgObjs)
  End Sub



  'Public Sub AnalyzeNode(node As SyntaxNode, semanticModel As SemanticModel, addDiagnostic As Action(Of Diagnostic), cancellationToken As CancellationToken) Implements ISyntaxNodeAnalyzer(Of SyntaxKind).AnalyzeNode
  '  Throw New NotImplementedException()
  'End Sub
End Class

