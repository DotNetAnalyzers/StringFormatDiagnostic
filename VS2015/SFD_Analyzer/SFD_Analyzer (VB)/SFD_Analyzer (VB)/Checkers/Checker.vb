Imports SFD_Analyzer
Imports RoslynExts.VB
Imports SFD.Analysis
Imports SFD.Analysis.VB
Imports SFD.Common


Namespace Global.SFD.Analysis
  Public Class Checker
    Implements IChecker

    Private A As IDiagnosticAnalyser
    Private I As DiagnosticInfo
    Friend Sub New(DA As IDiagnosticAnalyser, DI As DiagnosticInfo)
      A = DA
      I = DI
    End Sub

    Public Sub Check(method As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext, FIndex As Integer, Args As IEnumerable(Of Object)) Implements IChecker.Check
      If method IsNot Nothing Then _Shared_Checker_(A, method, context, FIndex, Args)
    End Sub


    Function CreateDiagnostic(description As DiagnosticDescriptor,
                           context As SyntaxNodeAnalysisContext,
                           ts As TextSpan,
                           sk As SFD.StringFormat.Span,
                           msg As String) As Diagnostic
      Return Diagnostic.Create(description, Location.Create(context.SemanticModel.SyntaxTree,
                          TextSpan.FromBounds(ts.Start + sk.Start + 1, ts.Start + sk.Finish + 1)), msg)
    End Function
    Sub CreateDiagnosticReport(description As DiagnosticDescriptor,
                           context As SyntaxNodeAnalysisContext,
                           ts As TextSpan,
                           sk As SFD.StringFormat.Span,
                           msg As String)
      context.ReportDiagnostic(CreateDiagnostic(description, context, ts, sk, msg))
    End Sub

    Private Sub UnusedArgs(context As SyntaxNodeAnalysisContext, unused As IEnumerable(Of Issues.Unused_Arg), args As IEnumerable(Of ArgumentSyntax))
      For Each u In unused.AsParallel
        Dim l = args(u.Value).GetLocation
        context.ReportDiagnostic(Diagnostic.Create(u.ID, I.Cat, u.Text, DiagnosticSeverity.Info, DiagnosticSeverity.Info, True, 0,,, l))
      Next
    End Sub

    'Sub AddError(id As String, msg As String, context As SyntaxNodeAnalysisContext, fs As TextSpan, sk As StringFormat.Span,
    '             Optional atSource As Boolean = False)
    '  Dim r As New DiagnosticDescriptor(id, "", msg, I.Cat, DiagnosticSeverity.Error, True)
    '  CreateDiagnosticReport(r, context, fs, If(atSource, New SFD.StringFormat.Span(sk.Source, 0, fs.Length), sk), msg)
    'End Sub

    'Sub AddWarning(id As String, msg As String, context As SyntaxNodeAnalysisContext, fs As TextSpan, sk As StringFormat.Span,
    '               Optional atSource As Boolean = False)
    '  Dim r As New DiagnosticDescriptor(id, "", msg, I.Cat, DiagnosticSeverity.Warning, True)
    '  CreateDiagnosticReport(r, context, fs, If(atSource, New StringFormat.Span(sk.Source, 0, fs.Length), sk), msg)
    'End Sub

    'Sub AddHidden(id As String, msg As String, context As SyntaxNodeAnalysisContext, fs As TextSpan, sk As StringFormat.Span,
    '              Optional atSource As Boolean = False)
    '  Dim r As New DiagnosticDescriptor(id, "", msg, I.Cat, DiagnosticSeverity.Hidden, True)
    '  CreateDiagnosticReport(r, context, fs, If(atsource, New StringFormat.Span(sk.Source, 0, fs.Length), sk), msg)
    'End Sub

    'Sub AddInfo(id As String, msg As String, context As SyntaxNodeAnalysisContext, fs As TextSpan, sk As StringFormat.Span,
    '             Optional atSource As Boolean = False)
    '  Dim r As New DiagnosticDescriptor(id, "", msg, I.Cat, DiagnosticSeverity.Info, True)
    '  CreateDiagnosticReport(r, context, fs, If(atSource, New StringFormat.Span(sk.Source, 0, fs.Length), sk), msg)
    'End Sub



    Private Sub _Shared_Checker_(TheAnalyser As IDiagnosticAnalyser,
                               MethodAccessed As MemberAccessExpressionSyntax,
                                context As SyntaxNodeAnalysisContext, FSIndex As Integer,
                                      ArgObjs As IEnumerable(Of Object))
      If (TheAnalyser Is Nothing) OrElse (MethodAccessed Is Nothing) Then Exit Sub
      If FSIndex < 0 Then Exit Sub
      Dim p = MethodAccessed.Parent.As(Of InvocationExpressionSyntax)
      Select Case ArgObjs.Count
        Case 0 ' Error
        Case Is > 0
          Dim Args = p.ArgumentList.Arguments
          If FSIndex >= Args.Count Then Exit Select
          Dim FormatTextArg = Args(FSIndex)
          If TypeOf FormatTextArg Is OmittedArgumentSyntax Then Exit Sub
          Dim TheFormatString = FormatTextArg.Try(Of SimpleArgumentSyntax)
          If TheFormatString IsNot Nothing Then
            Dim fp = ArgObjs.TakeWhile(Function(a) TypeOf a IsNot IFormatProvider)
            Dim sk = (ArgObjs.Count - fp.Count) - 1
            Dim ifp = CType(If(sk < 0, Nothing, ArgObjs(sk + 1)), IFormatProvider) ' CType(If(args.Count = 1, Nothing, ArgObjects(1)), IFormatProvider)
            ArgObjs = If(sk < 0, ArgObjs.Skip(FSIndex + 1), ArgObjs.Skip(sk))
            Select Case TheFormatString.Expression.VBKind
              Case SyntaxKind.StringLiteralExpression : _StringLiteral_(TheAnalyser, context, FormatTextArg, 1, ifp, ArgObjs, TheFormatString, sk, Args)
              Case SyntaxKind.IdentifierName : _AnIdentifier_(TheAnalyser, context, FormatTextArg, 0, ifp, ArgObjs, TheFormatString, sk, Args)
            End Select
          End If
      End Select
    End Sub




    Private Sub _StringLiteral_(fn As IDiagnosticAnalyser,
                                context As SyntaxNodeAnalysisContext,
                                fs As ArgumentSyntax,
                                FSIndex As Integer,
                                ifp As IFormatProvider,
                                ArgObjs As IEnumerable(Of Object),
                                TheFormatString As SimpleArgumentSyntax,
                                sk As Integer,
                                Args As SeparatedSyntaxList(Of ArgumentSyntax))
      Dim Parsing = SFD.StringFormat.StringFormat.Parse(StringFormat.SourceText.Create(fs.ToString.DeQuoted))
      Dim ReportedIssues = fn.Analyse(context.CancellationToken, Parsing, "", 1, ifp, ArgObjs)
      For Each _Issue_ In ReportedIssues.AsParallel
        Select Case _Issue_.Kind
          Case IssueKinds.Error : AddError(i,_Issue_.ID, _Issue_.Text, context, TheFormatString.Span, _Issue_.Span.Span)
          Case IssueKinds.Hidden : AddHidden(i,_Issue_.ID, _Issue_.Text, context, TheFormatString.Span, _Issue_.Span.Span)
          Case IssueKinds.Info : If TypeOf _Issue_ Is Unused_Arg Then Continue For
            AddInfo(i,_Issue_.ID, _Issue_.Text, context, TheFormatString.Span, _Issue_.Span.Span)
          Case IssueKinds.Warning : AddWarning(i,_Issue_.ID, _Issue_.Text, context, TheFormatString.Span, _Issue_.Span.Span)
        End Select
      Next
      Dim UseForParameterArgs = If(sk < 0, Args.Skip(FSIndex + 0), Args.Skip(sk))
      UnusedArgs(context, ReportedIssues.OfType(Of Unused_Arg), UseForParameterArgs)
    End Sub

    Private Sub _AnIdentifier_(fn As IDiagnosticAnalyser,
                               context As SyntaxNodeAnalysisContext,
                               fs As ArgumentSyntax,
                               FSIndex As Integer,
                               ifp As IFormatProvider,
                               ArgObjs As IEnumerable(Of Object),
                               TheFormatString As SimpleArgumentSyntax,
                               sk As Integer,
                               Args As SeparatedSyntaxList(Of ArgumentSyntax))
      Dim ThisIdentifier = CType(TheFormatString.Expression, IdentifierNameSyntax)
      If ThisIdentifier Is Nothing Then Exit Sub
      Dim ConstValue = context.SemanticModel.GetConstantValue(ThisIdentifier, context.CancellationToken)
      If ConstValue.HasValue = False Then Exit Sub
      Dim FoundSymbol = context.SemanticModel.LookupSymbols(TheFormatString.Expression.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
      Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, VariableDeclaratorSyntax)
      If VariableDeclarationSite Is Nothing Then Exit Sub
      Dim TheValueOfTheVariable = VariableDeclarationSite.Initializer.Value
      Dim ReportedIssues As IEnumerable(Of Base_Issue)
      If FoundSymbol.IsExtern Then
        ' Use usage site for location of Warings, ignore the yield ranges and use the span of ThisIdentifier.
        Dim s = SFD.StringFormat.StringFormat.Parse(StringFormat.SourceText.Create(ConstValue.Value.ToString))
        ReportedIssues = fn.Analyse(context.CancellationToken, s, "", 1, ifp, ArgObjs)
        For Each _Issue_ In ReportedIssues.AsParallel
          Select Case _Issue_.Kind
            Case IssueKinds.Error : AddError(i,_Issue_.ID, _Issue_.Text, context, TheFormatString.Span, _Issue_.Span.Span, True)
            Case IssueKinds.Hidden : AddHidden(i,_Issue_.ID, _Issue_.Text, context, TheFormatString.Span, _Issue_.Span.Span, True)
            Case IssueKinds.Info : If TypeOf _Issue_ Is Unused_Arg Then Continue For
              AddInfo(i,_Issue_.ID, _Issue_.Text, context, TheFormatString.Span, _Issue_.Span.Span, True)
            Case IssueKinds.Warning : AddWarning(i,_Issue_.ID, _Issue_.Text, context, TheFormatString.Span, _Issue_.Span.Span, True)
          End Select
        Next
      Else
        ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
        Dim s = SFD.StringFormat.StringFormat.Parse(StringFormat.SourceText.Create(ConstValue.Value.ToString))
        ReportedIssues = fn.Analyse(context.CancellationToken, s, "", 1, ifp, ArgObjs)
        For Each _Issue_ In ReportedIssues.AsParallel
          Select Case _Issue_.Kind
            Case IssueKinds.Error : AddError(i,_Issue_.ID, _Issue_.Text, context, TheValueOfTheVariable.Span, _Issue_.Span.Span)
            Case IssueKinds.Hidden : AddHidden(i,_Issue_.ID, _Issue_.Text, context, TheValueOfTheVariable.Span, _Issue_.Span.Span)
            Case IssueKinds.Info : If TypeOf _Issue_ Is Unused_Arg Then Continue For
              AddInfo(i,_Issue_.ID, _Issue_.Text, context, TheValueOfTheVariable.Span, _Issue_.Span.Span)
            Case IssueKinds.Warning : AddWarning(I, _Issue_.ID, _Issue_.Text, context, TheValueOfTheVariable.Span, _Issue_.Span.Span)
          End Select
        Next
      End If
      Dim UseForParameterArgs = If(sk < 0, Args.Skip(FSIndex + 1), Args.Skip(sk))
      UnusedArgs(context, ReportedIssues.OfType(Of Unused_Arg), UseForParameterArgs)
    End Sub


  End Class



End Namespace
