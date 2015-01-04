Option Infer On
Imports System.Runtime.CompilerServices
Imports AdamSpeight2008.StringFormatDiagnostics.VisualBasic
Imports SFD
Imports SFD.Analysis
Imports SFD.Analysis.VB

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class SFD_AnalyzerAnalyzer
  Inherits DiagnosticAnalyzer

  Shared _DI_ As New DiagnosticInfo

  Friend Shared Rule As New DiagnosticDescriptor(_DI_.ID, _DI_.Title, _DI_.MSG, _DI_.Cat, DiagnosticSeverity.Warning, isEnabledByDefault:=True)

  Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
    Get
      Return ImmutableArray.Create(Rule)
    End Get
  End Property

  Dim _C_ As Checkers_VB

  Public Overrides Sub Initialize(context As AnalysisContext)
    SFD.Analysers.Initialise()
    _C_ = New Checkers_VB(_DI_, SFD.Analysers.DAnalysers)
    context.RegisterSyntaxNodeAction(Of SyntaxKind)(AddressOf AnalyzeSymbol, SyntaxKind.SimpleMemberAccessExpression)
  End Sub

  Public Sub AnalyzeSymbol(context As SyntaxNodeAnalysisContext)
    ' Does the node physically exist?
    If context.Node Is Nothing Then Exit Sub
    ' Is the Node of the correct kind?
    If context.Node.IsKind(SyntaxKind.SimpleMemberAccessExpression) = False Then Exit Sub
    ' Cast the node the Syntax Node
    Dim method = TryCast(context.Node, Syntax.MemberAccessExpressionSyntax)
    ' Was it successful?
    If method Is Nothing Then Exit Sub
    ' Is the operator '.' ?
    If method.OperatorToken.VBKind <> SyntaxKind.DotToken Then Exit Sub
    ' What is the type of the Callee
    Dim Callee_Type = method.CalledOnType(context)
    Dim _TypeName_ = If(Callee_Type Is Nothing, "", Callee_Type.ToFullyQualifiedName)
    If _TypeName_ = "" Then Exit Sub
    'Dim _TypeNameA() = _TypeName_.Split(","c)
    Dim _InvokeExpr_ = TryCast(method.Parent, InvocationExpressionSyntax)
    If _InvokeExpr_ Is Nothing Then Exit Sub

    Dim _Args_ = _InvokeExpr_.ArgumentList
    Dim _ArgObjs_ = _Args_.GetArgumentAsObjects(context)
    Dim _ArgTypeNames_ = _Args_.GetArgumentTypesNames(context).ToArray

    Dim _Analysis = SFD.Analysers.Analysis

    Dim possibles = (From a In _Analysis.AsParallel.AsOrdered
                     Where a.TypeName = _TypeName_
                     Order By a.ParamTypes.Count Descending).ToArray

    For Each qp As AB In possibles.Where(Function(p) _ArgTypeNames_.BeginsWith(p.ParamTypes) AndAlso
                                                     _C_._DictOfAnalysers.ContainsKey(p.Analyser)).
                                   Select(Function(p) New AB(_C_._DictOfAnalysers(p.Analyser), p))
      qp.A.Check(method, context, qp.B.FIndex, _ArgObjs_)
    Next

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
      Public Function AnyIsNull(Of T As Class)(a() As T) As Boolean
        If a Is Nothing Then Return True
        For i = 0 To a.Count - 1
          If a(i) Is Nothing Then Return True
        Next
        Return False
      End Function
      <Extension>
      Public Function BeginsWith(Of T As IComparable(Of T))(xs As IList(Of T), ys As IList(Of T)) As Boolean
        If {xs, ys}.AnyIsNull Then Return False
        If xs.Count < ys.Count Then Return False
        For i = 0 To ys.Count - 1
          If xs(i).CompareTo(ys(i)) <> 0 Then Return False
        Next
        Return True
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

