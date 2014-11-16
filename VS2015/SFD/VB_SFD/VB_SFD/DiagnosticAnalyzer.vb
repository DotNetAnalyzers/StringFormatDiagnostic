Option Infer On
Imports System.Runtime.CompilerServices
Imports AdamSpeight2008.StringFormatDiagnostics.VisualBasic
Imports AdamSpeight2008.StringFormatDiagnostics
Imports AdamSpeight2008.StringFormatDiagnostics.Analysers
Imports AdamSpeight2008.StringFormatDiagnostics.Results
Imports AdamSpeight2008.StringFormatDiagnostics.Common
Imports AdamSpeight2008.StringFormatDiagnostics.VisualBasic.Exts
Imports AdamSpeight2008


<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class VB_SFDAnalyzer
  Inherits DiagnosticAnalyzer

  Public Const DiagnosticId = "VB_SFD"
  Friend Const Title = "String Format Diagnostics"
  Friend Const MessageFormat = "{0}"
  Friend Const Category = "Diagnostics"
  Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault:=True)
  Private Shared _DictOfAnalysers As New Dictionary(Of String, Action(Of MemberAccessExpressionSyntax, SyntaxNodeAnalysisContext, Integer, IEnumerable(Of Object)))

  'Sub New()
  'End Sub
  Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
    Get
      Return ImmutableArray.Create(Rule)
    End Get
  End Property
  Private _Analysis As New List(Of SFD_Diag)
  Public Overrides Sub Initialize(context As AnalysisContext)
    If _DictOfAnalysers.Count <> 0 Then Exit Sub
    _DictOfAnalysers = New Dictionary(Of String, Action(Of MemberAccessExpressionSyntax, SyntaxNodeAnalysisContext, Integer, IEnumerable(Of Object))) From
     {{"SF", AddressOf Check_FormatString}, {"Num", AddressOf Check_Numeric_ToString}, {"Date", AddressOf Check_DateTime_ToString},
      {"Enum", AddressOf Check_Enum_ToString}, {"DateOff", AddressOf Check_DateTimeOffset_ToString}, {"TS", AddressOf Check_TimeSpan_ToString}}
    ' TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
    Dim the_file = Xml.Linq.XDocument.Load("analyserlist.xml")
    If the_file Is Nothing Then Exit Sub
    Dim a = the_file.<Analyser>
    For Each an In a
      Dim index = -1
      If Integer.TryParse(an.@Index, index) Then
        Dim typename = a.@TypeName
        Dim args = a.<Arg>.Select(Function(x) x.@Type).ToArray
        Dim sfd As New SFD_Diag(a.@TypeName, a.@MethodName, index, a.@Use, args)
        _Analysis.Add(sfd)
      End If
    Next
    context.RegisterSyntaxNodeAction(Of SyntaxKind)(AddressOf AnalyzeNode, SymbolKind.NamedType)

  End Sub

  Public Sub AnalyzeNode(context As SyntaxNodeAnalysisContext)
    If Not context.Node.IsKind(SyntaxKind.SimpleMemberAccessExpression) Then Exit Sub
    Dim method = DirectCast(context.Node, MemberAccessExpressionSyntax)
    If method.OperatorToken.VBKind <> SyntaxKind.DotToken Then Exit Sub
    Dim _MethodName = method.Name.ToString
    If _MethodName = "" Then Exit Sub
    Dim _CalledOnObjOfType = method.CalledOnType(context)
    Dim _TypeName = If(_CalledOnObjOfType Is Nothing, "", _CalledOnObjOfType.ToFullyQualifiedName)
    Dim _TypeNameA() = _TypeName.Split("."c)
    Dim _InvokeExpr = TryCast(method.Parent, InvocationExpressionSyntax)
    If _InvokeExpr Is Nothing Then Exit Sub
    Dim Args = _InvokeExpr.ArgumentList
    Dim ArgObjs = Args.GetArgumentAsObjects(context)
    Dim ArgTypeNames = Args.GetArgumentTypesNames(context).ToArray

    ' Try to see if it is one the simple ones
    Dim possibles = (From a In _Analysis.AsParallel.AsOrdered Where a.TypeName = _TypeName Order By a.ParamTypes.Count Descending).ToArray
    For Each qp In possibles.Where(Function(p) ArgTypeNames.BeginsWith(p.ParamTypes) AndAlso _DictOfAnalysers.ContainsKey(p.Analyser)).
                                         Select(Function(p) New With {.a = _DictOfAnalysers(p.Analyser), .p = p})
      qp.a.Invoke(method, context, qp.p.FIndex, ArgObjs)
    Next
  End Sub



  Public Sub Check_FormatString(node As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    _Shared_Checker_(AddressOf AnalyseFormatString, node, context, fsi, ArgObjs)
  End Sub

  Public Sub Check_TimeSpan_ToString(node As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_TimeSpan_ToString, node, context, fsi, ArgObjs)
  End Sub

  Public Sub Check_Enum_ToString(node As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_Enum_ToString, node, context, fsi, ArgObjs)
  End Sub

  Public Sub Check_DateTimeOffset_ToString(node As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_DateTimeOffset_ToString, node, context, fsi, ArgObjs)
  End Sub

  Public Sub Check_DateTime_ToString(node As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Analyse_DateTime_ToString, node, context, fsi, ArgObjs)
  End Sub

  Public Sub Check_Numeric_ToString(node As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext, fsi As Integer, ArgObjs As IEnumerable(Of Object))
    If node Is Nothing Then Exit Sub
    _Shared_Checker_(AddressOf Numerics.Analyse_Numeric_ToString, node, context, fsi, ArgObjs)
  End Sub

  Private Sub UnusedArgs(context As SyntaxNodeAnalysisContext, unused As IEnumerable(Of Warnings.UnusedArg), args As IEnumerable(Of ArgumentSyntax))
    For Each u In unused
      context.ReportDiagnostic(Diagnostic.Create(DiagnosticId, Category, "Argument Unused", DiagnosticSeverity.Warning, True, 1, False,,, args(u.ArgIndex).GetLocation))
    Next
  End Sub

  Sub AddError(msg As String, context As SyntaxNodeAnalysisContext, offset As Integer, s As IndexSpan)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Error, True)
    Dim q = Diagnostic.Create(DiagnosticId, Category, String.Format(MessageFormat, msg), DiagnosticSeverity.Error, True, 0, False, Title,, Location.Create(context.SemanticModel.SyntaxTree, TextSpan.FromBounds(offset + s.Index + 1, offset + s.Index + s.Span)))
    'd(q)
    context.ReportDiagnostic(Diagnostic.Create(r, Location.Create(context.SemanticModel.SyntaxTree, TextSpan.FromBounds(offset + s.Index + 1, offset + s.Index + s.Span)), msg))
  End Sub
  Sub AddError(msg As String, context As SyntaxNodeAnalysisContext, index As Integer)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Error, True)
    context.ReportDiagnostic(Diagnostic.Create(r, Location.Create(context.SemanticModel.SyntaxTree, TextSpan.FromBounds(index + 1, index + 1)), msg))
  End Sub
  Sub AddWarning(msg As String, context As SyntaxNodeAnalysisContext, s As TextSpan)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Warning, True)
    context.ReportDiagnostic(Diagnostic.Create(r, Location.Create(context.SemanticModel.SyntaxTree, s), msg))
  End Sub


  Public Sub AddErrorAtSource(context As SyntaxNodeAnalysisContext, node As SyntaxNode, offset As Integer, endoffset As Integer, msg As String)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Error, True)
    context.ReportDiagnostic(Diagnostic.Create(r, Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), msg))
  End Sub
  Public Sub AddWarningAtSource(context As SyntaxNodeAnalysisContext, node As SyntaxNode, offset As Integer, endoffset As Integer, msg As String)
    Dim r As New DiagnosticDescriptor(DiagnosticId, "", MessageFormat, Category, DiagnosticSeverity.Warning, True)

    context.ReportDiagnostic(Diagnostic.Create(r,
                           Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), msg))
  End Sub

  Private Sub _Shared_Checker_(fn As Func(Of CancellationToken, String, Integer, IFormatProvider, IEnumerable(Of Object), Base_Result),
                             node As MemberAccessExpressionSyntax,
                              context As SyntaxNodeAnalysisContext, FSIndex As Integer,
                                    Args As IEnumerable(Of Object))
    If (fn Is Nothing) OrElse (node Is Nothing) Then Exit Sub
    If FSIndex < 0 Then Exit Sub
    Dim p = CType(node.Parent, InvocationExpressionSyntax)
    Select Case Args.Count
      Case 0 ' Error
      Case Is > 0
        Dim aa = p.ArgumentList.Arguments
        If FSIndex >= aa.Count Then Exit Select
        Dim fs = aa(FSIndex) 'args.First
        If TypeOf fs Is OmittedArgumentSyntax Then Exit Sub
        Dim TheFormatString = CType(fs, SimpleArgumentSyntax)
        If TheFormatString IsNot Nothing Then
          Dim fp = Args.TakeWhile(Function(a) TypeOf a IsNot IFormatProvider)
          Dim sk = (Args.Count - fp.Count) - 1
          Dim ifp = CType(If(sk < 0, Nothing, Args(sk + 1)), IFormatProvider) ' CType(If(args.Count = 1, Nothing, ArgObjects(1)), IFormatProvider)
          Args = If(sk < 0, Args.Skip(FSIndex + 1), Args.Skip(sk))
          Select Case TheFormatString.Expression.VBKind
            Case SyntaxKind.StringLiteralExpression
              Dim ReportedIssues = fn(context.CancellationToken, fs.ToString.DeQuoted, 1, ifp, Args)
              For Each _Error_ In ReportedIssues.Errors
                If TypeOf _Error_ Is Errors.Parse_ErrorAtIndex Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorAtIndex) : AddError(es.ToString(), context, TheFormatString.SpanStart + es.Index)
                If TypeOf _Error_ Is Errors.Parse_ErrorSpan Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorSpan) : AddError(es.ToString(), context, TheFormatString.SpanStart, es.Index)
              Next

              For Each _Warning_ In ReportedIssues.Warnings
                If TypeOf _Warning_ IsNot Warnings.UnusedArg Then AddWarning(_Warning_.ToString, context, TheFormatString.Span)
              Next
              UnusedArgs(context, ReportedIssues.Warnings.OfType(Of Warnings.UnusedArg), If(sk < 0, aa.Skip(FSIndex + 1), aa.Skip(sk)))
            Case SyntaxKind.IdentifierName
              Dim ThisIdentifier = CType(TheFormatString.Expression, IdentifierNameSyntax)
              If ThisIdentifier Is Nothing Then Exit Sub
              Dim ConstValue = context.SemanticModel.GetConstantValue(ThisIdentifier, context.CancellationToken)
              If ConstValue.HasValue = False Then Exit Sub
              Dim FoundSymbol = context.SemanticModel.LookupSymbols(TheFormatString.Expression.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
              Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, VariableDeclaratorSyntax)
              If VariableDeclarationSite Is Nothing Then Exit Sub
              Dim TheValueOfTheVariable = VariableDeclarationSite.Initializer.Value
              ' Debugger.Break()
              If FoundSymbol.IsExtern Then
                ' Use usage site for location of Warings, ignore the yield ranges and use the span of ThisIdentifier.
                Dim ReportedIssues = fn(context.CancellationToken, ConstValue.Value.ToString, 0, ifp, Args)
                For Each _Error_ In ReportedIssues.Errors
                  If TypeOf _Error_ Is Errors.Parse_ErrorAtIndex Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorAtIndex) : AddErrorAtSource(context, fs, es.Index, 1, es.ToString)
                  If TypeOf _Error_ Is Errors.Parse_ErrorSpan Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorSpan) : AddErrorAtSource(context, fs, es.Index.Index, es.Index.Index + es.Index.Span + 1, es.ToString)
                Next
                For Each _Warning_ In ReportedIssues.Warnings
                  If TypeOf _Warning_ IsNot Warnings.UnusedArg Then AddWarningAtSource(context, fs, 0, 1, _Warning_.ToString)
                Next
                UnusedArgs(context, ReportedIssues.Warnings.OfType(Of Warnings.UnusedArg), If(sk < 0, aa.Skip(FSIndex + 1), aa.Skip(sk)))

              Else
                ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                Dim ReportedIssues = fn(context.CancellationToken, ConstValue.Value.ToString, 0, ifp, Args)
                For Each _Error_ In ReportedIssues.Errors
                  If TypeOf _Error_ Is Errors.Parse_ErrorAtIndex Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorAtIndex) : AddErrorAtSource(context, TheValueOfTheVariable, es.Index + 1, es.Index + 1, es.ToString)
                  If TypeOf _Error_ Is Errors.Parse_ErrorSpan Then Dim es = DirectCast(_Error_, Errors.Parse_ErrorSpan) : AddErrorAtSource(context, TheValueOfTheVariable, es.Index.Index + 1, es.Index.Index + es.Index.Span, es.ToString)
                Next
                For Each _Warning_ In ReportedIssues.Warnings
                  If TypeOf _Warning_ IsNot Warnings.UnusedArg Then AddWarningAtSource(context, TheValueOfTheVariable, 0, 1, _Warning_.ToString)
                Next
                UnusedArgs(context, ReportedIssues.Warnings.OfType(Of Warnings.UnusedArg), If(sk < 0, aa.Skip(FSIndex + 1), aa.Skip(sk)))

              End If
          End Select
        End If
    End Select
  End Sub

End Class


Namespace Global.AdamSpeight2008.StringFormatDiagnostics
  Namespace VisualBasic
    Public Module Exts

      <Extension>
      Public Function ArgumentType(arg As ArgumentSyntax, context As SyntaxNodeAnalysisContext) As ITypeSymbol
        Try
          Return context.SemanticModel.GetTypeInfo(CType(arg, SimpleArgumentSyntax).Expression, context.CancellationToken).Type
        Catch ex As Exception
        End Try
        Return Nothing
      End Function

      <Extension>
      Public Function GetArgumentTypes(args As ArgumentListSyntax, context As SyntaxNodeAnalysisContext) As IEnumerable(Of ITypeSymbol)
        If (args Is Nothing) Then Return Enumerable.Empty(Of ITypeSymbol)

        Return args.Arguments.Select(Function(arg) arg.ArgumentType(context))
      End Function

      <Extension>
      Public Function GetArgumentTypesNames(args As ArgumentListSyntax, context As SyntaxNodeAnalysisContext) As IEnumerable(Of String)
        If (args Is Nothing) Then Return Enumerable.Empty(Of String)
        Return args.GetArgumentTypes(context).Select(Function(tsym) If(tsym Is Nothing, String.Empty, tsym.ToFullyQualifiedName))
      End Function

      <Extension>
      Public Iterator Function GetArgumentAsObjects(args As ArgumentListSyntax, context As SyntaxNodeAnalysisContext) As IEnumerable(Of Object)
        If (args Is Nothing) Then Exit Function
        Dim ArgTypes = args.GetArgumentTypes(context)
        For i = 0 To args.Arguments.Count - 1
          Dim ov As Object
          Dim Arg = CType(args.Arguments(i), SimpleArgumentSyntax)
          If TypeOf Arg.Expression Is IdentifierNameSyntax Then
            ov = IdentifierValue(DirectCast(Arg.Expression, IdentifierNameSyntax), context)
          Else
            Try
              ov = Convert.ChangeType(Arg.DescendantTokens.First.Value, Type.GetType(ArgTypes(i).ToFullyQualifiedName, False))
            Catch ex As Exception
              ov = Nothing
            End Try
          End If
          Yield ov
        Next
      End Function

      <Extension>
      Function IsExternal(sn As SyntaxNode, context As SyntaxNodeAnalysisContext) As Boolean
        If (sn Is Nothing) Then Return True
        Return context.SemanticModel.GetSymbolInfo(sn, context.CancellationToken).Symbol.IsExtern
      End Function

      <Extension>
      Function IdentifierValue(ThisIdentifier As IdentifierNameSyntax, context As SyntaxNodeAnalysisContext) As Object
        If ThisIdentifier Is Nothing Then Return Nothing
        Dim FoundSymbol = context.SemanticModel.LookupSymbols(ThisIdentifier.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
        Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, VariableDeclaratorSyntax)
        If VariableDeclarationSite Is Nothing Then Return Nothing
        If VariableDeclarationSite.Initializer Is Nothing Then Return Nothing
        If VariableDeclarationSite.Initializer.Value Is Nothing Then Return Nothing

        'If AnyIsNull(Of Object)( {VariableDeclarationSite,VariableDeclarationSite.Initializer,VariableDeclarationSite.Initializer.Value }) THen Return nothing
        Dim f = VariableDeclarationSite.Initializer.Value.DescendantTokens.First
        'If f Is Nothing Then Return nothing 
        Dim TheValueOfTheVariable = f.Value
        Return Convert.ChangeType(TheValueOfTheVariable, Type.GetType(context.SemanticModel.GetTypeInfo(ThisIdentifier, context.CancellationToken).Type.ToFullyQualifiedName, False))
      End Function

      <Extension>
      Public Function CalledOnType(n As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext) As INamedTypeSymbol
        If (n Is Nothing) Then Return Nothing
        Dim s = context.SemanticModel.GetSymbolInfo(n, context.CancellationToken).Symbol
        Return If(s Is Nothing, Nothing, s.ContainingType)
      End Function

      <Extension>
      Public Function ToFullyQualifiedName(s As ISymbol) As String
        If s Is Nothing Then Return String.Empty
        Return s.ToDisplayString(New SymbolDisplayFormat(typeQualificationStyle:=SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces))
      End Function

    End Module

  End Namespace

End Namespace
