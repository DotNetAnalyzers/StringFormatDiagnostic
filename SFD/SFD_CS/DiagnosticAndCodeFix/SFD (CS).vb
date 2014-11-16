Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis.Diagnostics
Imports AdamSpeight2008.StringFormatDiagnostics.Common.SFD
Imports AdamSpeight2008
Imports AdamSpeight2008.StringFormatDiagnostics
Imports AdamSpeight2008.StringFormatDiagnostics.Errors
Imports AdamSpeight2008.StringFormatDiagnostics.Results
Imports AdamSpeight2008.StringFormatDiagnostics.CSharp.Exts
Imports AdamSpeight2008.StringFormatDiagnostics.Common
Imports AdamSpeight2008.StringFormatDiagnostics.Analysers
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.CSharp.Syntax

<DiagnosticAnalyzer(LanguageNames.CSharp)>
Public Class DiagnosticAnalyzer
  Implements ISyntaxNodeAnalyzer(Of SyntaxKind)


  Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Description, MessageFormat, Category, DiagnosticSeverity.Warning, True)
  Public ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor) Implements IDiagnosticAnalyzer.SupportedDiagnostics
    Get
      Return ImmutableArray.Create(Rule)
    End Get
  End Property

  Private ReadOnly Property SyntaxKindsOfInterest As ImmutableArray(Of SyntaxKind) Implements ISyntaxNodeAnalyzer(Of SyntaxKind).SyntaxKindsOfInterest
    Get
      Return ImmutableArray.Create(SyntaxKind.SimpleMemberAccessExpression)
    End Get
  End Property

  Shared Sub New()
    Initialise()
  End Sub
  Private Shared _DictOfAnalysers As New Concurrent.ConcurrentDictionary(Of String, Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object)))

  Sub New()
    If _DictOfAnalysers.Count <> 0 Then Exit Sub
    _DictOfAnalysers = New Concurrent.ConcurrentDictionary(Of String, Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object))) From
     {{"SF", AddressOf Check_FormatString}, {"Num", AddressOf Check_Numeric_ToString}, {"Date", AddressOf Check_DateTime_ToString},
      {"Enum", AddressOf Check_Enum_ToString}, {"DateOff", AddressOf Check_DateTimeOffset_ToString}, {"TS", AddressOf Check_TimeSpan_ToString}}
  End Sub

  Public Sub AnalyzeNode(node As SyntaxNode,
                       semanticModel As SemanticModel,
                       addDiagnostic As Action(Of Diagnostic),
                             options As AnalyzerOptions,
                                  ct As CancellationToken) Implements ISyntaxNodeAnalyzer(Of SyntaxKind).AnalyzeNode
    Dim x = CType(node, MemberAccessExpressionSyntax)
    If x Is Nothing Then Exit Sub
    If x.OperatorToken.ValueText = "." Then
      Dim _MethodName = x.Name.ToString
      If _MethodName = "" Then Exit Sub
      Dim _CalledOnObjOfType = x.CalledOnType(semanticModel, ct)
      Dim _TypeName = If(_CalledOnObjOfType Is Nothing, "", _CalledOnObjOfType.ToFullyQualifiedName)
      Dim _TypeNameA() = _TypeName.Split("."c)
      Dim _InvokeExpr = TryCast(x.Parent, InvocationExpressionSyntax)
      If _InvokeExpr Is Nothing Then Exit Sub

      Dim Args = _InvokeExpr.ArgumentList


      Dim ArgObjs = Args.GetArgumentAsObjects(semanticModel, ct)

      ''Dim ArgTypes = Args.GetArgumentTypes(semanticModel, cancellationToken)
      Dim ArgTypeNames = Args.GetArgumentTypesNames(semanticModel, ct).ToArray
      'If (_MethodName = "Format") AndAlso (_TypeName = "System.String") Then
      '    Dim holes = Yield_ArgHoles(ArgObjs(0)).ToArray
      'End If
      Dim used As New HashSet(Of Integer)
      ' Try to see if it is one the simple ones
      Dim possibles = (From a In Analysis.AsParallel.AsOrdered Where a.TypeName = _TypeName Order By a.ParamTypes.Count Descending).ToArray
      For Each qp In possibles.Where(Function(p) ArgTypeNames.BeginsWith(p.ParamTypes) AndAlso _DictOfAnalysers.ContainsKey(p.Analyser)).
                                           Select(Function(p) New With {.a = _DictOfAnalysers(p.Analyser), .p = p})
        qp.a.Invoke(x, semanticModel, addDiagnostic, ct, qp.p.FIndex, ArgObjs)
        used.Add(qp.p.FIndex)
      Next
      '
      For Each u In Enumerable.Range(0, Args.Arguments.Count).Except(used)
        addDiagnostic(Diagnostic.Create(DiagnosticId, Category, "Argument Unused", DiagnosticSeverity.Warning, True, 1, False,,, Args.Arguments(u + 1).GetLocation))
      Next
    End If

  End Sub
  Public Const DiagnosticId = "SFD"
  Friend Const Description = "String Format Diagnostics"
  Friend Const MessageFormat = "{0}"
  Friend Const Category = "Diagnostics"

  Sub AddError(msg As String, d As Action(Of Diagnostic), st As SyntaxTree, offset As Integer, s As IndexSpan)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Error, True)
    Dim q = Diagnostic.Create(DiagnosticId, Category, String.Format(MessageFormat, msg), DiagnosticSeverity.Error, True, 0, False, Description,, Location.Create(st, TextSpan.FromBounds(offset + s.Index + 1, offset + s.Index + s.Span)))
    d(q)
    '   d(Diagnostic.Create(r, Location.Create(st, TextSpan.FromBounds(offset + s.Index + 1, offset + s.Index + s.Span)), msg))
  End Sub
  Sub AddError(msg As String, d As Action(Of Diagnostic), st As SyntaxTree, index As Integer)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Error, True)
    d(Diagnostic.Create(r, Location.Create(st, TextSpan.FromBounds(index + 1, index + 1)), msg))
  End Sub
  Sub AddWarning(msg As String, d As Action(Of Diagnostic), st As SyntaxTree, s As TextSpan)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Warning, True)
    d(Diagnostic.Create(r, Location.Create(st, s), msg))
  End Sub


  Public Sub AddErrorAtSource(d As Action(Of Diagnostic), node As SyntaxNode, offset As Integer, endoffset As Integer, msg As String)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Error, True)
    d(Diagnostic.Create(r, Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), msg))
  End Sub
  Public Sub AddWarningAtSource(d As Action(Of Diagnostic), node As SyntaxNode, offset As Integer, endoffset As Integer, msg As String)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Warning, True)

    d(Diagnostic.Create(r,
                               Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), msg))
  End Sub

  Private Sub UnusedArgs(diag As Action(Of Diagnostic), unused As IEnumerable(Of Warnings.UnusedArg), args As IEnumerable(Of ArgumentSyntax))
    For Each u In unused
      diag(Diagnostic.Create(DiagnosticId, Category, "Argument Unused", DiagnosticSeverity.Warning, True, 1, False,,, args(u.ArgIndex).GetLocation))
    Next
  End Sub
  Private Sub _Shared_Checker_(fn As Func(Of CancellationToken, String, Integer, IFormatProvider, IEnumerable(Of Object), Base_Result),
                                 node As MemberAccessExpressionSyntax,
                                   sm As SemanticModel,
                                 diag As Action(Of Diagnostic), ct As CancellationToken, FSIndex As Integer,
                                        Args As IEnumerable(Of Object))
    If AnyIsNull({fn, node, sm, diag}) Then Exit Sub
    If FSIndex < 0 Then Exit Sub
    Dim p = CType(node.Parent, InvocationExpressionSyntax)
    Select Case Args.Count
      Case 0 ' Error
      Case Is > 0
        Dim aa = p.ArgumentList.Arguments
        If FSIndex >= aa.Count Then Exit Select
        Dim fs = aa(FSIndex) 'args.First
        If fs.IsMissing Then Exit Sub
        Dim TheFormatString = CType(fs, ArgumentSyntax)
        If TheFormatString IsNot Nothing Then
          Dim fp = Args.TakeWhile(Function(a) TypeOf a IsNot IFormatProvider)
          Dim sk = (Args.Count - fp.Count) - 1
          Dim ifp = CType(If(sk < 0, Nothing, Args(sk + 1)), IFormatProvider) ' CType(If(args.Count = 1, Nothing, ArgObjects(1)), IFormatProvider)
          Args = If(sk < 0, Args.Skip(FSIndex + 1), Args.Skip(sk))
          Select Case TheFormatString.Expression.CSharpKind
            Case SyntaxKind.StringLiteralExpression
              Dim ReportedIssues = fn(ct, fs.ToString.DeQuoted, 1, ifp, Args)
              For Each _Error_ In ReportedIssues.Errors
                If TypeOf _Error_ Is Errors.Parse_ErrorAtIndex Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorAtIndex) : AddError(es.ToString(), diag, sm.SyntaxTree, TheFormatString.SpanStart + es.Index)
                If TypeOf _Error_ Is Errors.Parse_ErrorSpan Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorSpan) : AddError(es.ToString(), diag, sm.SyntaxTree, TheFormatString.SpanStart, es.Index)
              Next
              For Each _Warning_ In ReportedIssues.Warnings
                If TypeOf _Warning_ IsNot Warnings.UnusedArg Then AddWarning(_Warning_.ToString, diag, sm.SyntaxTree, TheFormatString.Span)
              Next
              UnusedArgs(diag, ReportedIssues.Warnings.OfType(Of Warnings.UnusedArg), If(sk < 0, aa.Skip(FSIndex + 1), aa.Skip(sk)))

            Case SyntaxKind.IdentifierName
              Dim ThisIdentifier = CType(TheFormatString.Expression, IdentifierNameSyntax)
              If ThisIdentifier Is Nothing Then Exit Sub
              Dim ConstValue = sm.GetConstantValue(ThisIdentifier, ct)
              If ConstValue.HasValue = False Then Exit Sub
              Dim FoundSymbol = sm.LookupSymbols(TheFormatString.Expression.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
              Dim qq = FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent
              Dim VariableDeclarationSite = TryCast(qq, VariableDeclarationSyntax).Variables(0)
              If VariableDeclarationSite Is Nothing Then Exit Sub
              Dim TheValueOfTheVariable = VariableDeclarationSite.Initializer.Value
              ' Debugger.Break()
              If FoundSymbol.IsExtern Then
                ' Use usage site for location of Warings, ignore the yield ranges and use the span of ThisIdentifier.
                Dim ReportedIssues = fn(ct, ConstValue.Value.ToString, 0, ifp, Args)
                For Each _Error_ In ReportedIssues.Errors
                  If TypeOf _Error_ Is Errors.Parse_ErrorAtIndex Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorAtIndex) : AddErrorAtSource(diag, fs, es.Index, 1, es.ToString)
                  If TypeOf _Error_ Is Errors.Parse_ErrorSpan Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorSpan) : AddErrorAtSource(diag, fs, es.Index.Index, es.Index.Index + es.Index.Span + 1, es.ToString)
                Next
                For Each _Warning_ In ReportedIssues.Warnings
                  If TypeOf _Warning_ IsNot Warnings.UnusedArg Then AddWarningAtSource(diag, fs, 0, 1, _Warning_.ToString)
                Next
                UnusedArgs(diag, ReportedIssues.Warnings.OfType(Of Warnings.UnusedArg), If(sk < 0, aa.Skip(FSIndex + 1), aa.Skip(sk)))

              Else
                ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                Dim ReportedIssues = fn(ct, ConstValue.Value.ToString, 0, ifp, Args)
                For Each _Error_ In ReportedIssues.Errors
                  If TypeOf _Error_ Is Errors.Parse_ErrorAtIndex Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorAtIndex) : AddErrorAtSource(diag, TheValueOfTheVariable, es.Index + 1, es.Index + 1, es.ToString)
                  If TypeOf _Error_ Is Errors.Parse_ErrorSpan Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorSpan) : AddErrorAtSource(diag, TheValueOfTheVariable, es.Index.Index + 1, es.Index.Index + es.Index.Span, es.ToString)
                Next
                For Each _Warning_ In ReportedIssues.Warnings
                  If TypeOf _Warning_ IsNot Warnings.UnusedArg Then AddWarningAtSource(diag, TheValueOfTheVariable, 0, 1, _Warning_.ToString)
                Next
                UnusedArgs(diag, ReportedIssues.Warnings.OfType(Of Warnings.UnusedArg), If(sk < 0, aa.Skip(FSIndex + 1), aa.Skip(sk)))

              End If
          End Select
        End If
    End Select
  End Sub

  Public Sub Check_FormatString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    _Shared_Checker_(AddressOf AnalyseFormatString, node, sm, addDiagnostic, ct, fsi, ArgObjs)
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



End Class
