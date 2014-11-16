Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft
Imports Microsoft.CodeAnalysis.CSharp.Syntax

Namespace Global.AdamSpeight2008.StringFormatDiagnostics
  Namespace CSharp
    Public Module Exts
      <Extension>
      Public Function ArgumentType(arg As ArgumentSyntax, sm As SemanticModel, ct As CancellationToken) As ITypeSymbol
        Try
          Return sm.GetTypeInfo(CType(arg, ArgumentSyntax).Expression, ct).Type
        Catch ex As Exception
        End Try
        Return Nothing
      End Function

      <Extension>
      Public Function GetArgumentTypes(args As ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of ITypeSymbol)
        If (args Is Nothing) OrElse (sm Is Nothing) Then Return Enumerable.Empty(Of ITypeSymbol)

        Return args.Arguments.Select(Function(arg) arg.ArgumentType(sm, ct))
      End Function

      <Extension>
      Public Function GetArgumentTypesNames(args As ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of String)
        If (args Is Nothing) OrElse (sm Is Nothing) Then Return Enumerable.Empty(Of String)
        Return args.GetArgumentTypes(sm, ct).Select(Function(tsym) If(tsym Is Nothing, String.Empty, tsym.ToFullyQualifiedName))
      End Function

      <Extension>
      Public Iterator Function GetArgumentAsObjects(args As ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of Object)
        If (args Is Nothing) OrElse (sm Is Nothing) Then Exit Function
        Dim ArgTypes = args.GetArgumentTypes(sm, ct)
        For i = 0 To args.Arguments.Count - 1
          Dim ov As Object
          Dim Arg = CType(args.Arguments(i), ArgumentSyntax)
          If TypeOf Arg.Expression Is IdentifierNameSyntax Then
            ov = IdentifierValue(DirectCast(Arg.Expression, IdentifierNameSyntax), sm, ct)
          Else
            Try
              ov = Convert.ChangeType(Arg.DescendantTokens.First.Value, Type.GetType(ArgTypes(i).ToFullyQualifiedName, False, True))
            Catch ex As Exception
              ov = Nothing
            End Try
          End If
          Yield ov
        Next
      End Function

      <Extension>
      Function IsExternal(sn As SyntaxNode, sm As SemanticModel, ct As CancellationToken) As Boolean
        If AnyIsNull(Of Object)({sn, sm}) Then Return True
        Return sm.GetSymbolInfo(sn, ct).Symbol.IsExtern
      End Function

      <Extension>
      Function IdentifierValue(ThisIdentifier As IdentifierNameSyntax, sm As SemanticModel, ct As CancellationToken) As Object
        If AnyIsNull(Of Object)({ThisIdentifier, sm}) Then Return Nothing

        Dim FoundSymbol = sm.LookupSymbols(ThisIdentifier.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
        Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, CodeAnalysis.CSharp.Syntax.VariableDeclaratorSyntax)
        If VariableDeclarationSite Is Nothing Then Return Nothing
        If VariableDeclarationSite.Initializer Is Nothing Then Return Nothing
        If VariableDeclarationSite.Initializer.Value Is Nothing Then Return Nothing
        Dim f = VariableDeclarationSite.Initializer.Value.DescendantTokens.First
        'If f Is Nothing Then Return nothing 
        Dim TheValueOfTheVariable = f.Value
        Return Convert.ChangeType(TheValueOfTheVariable, Type.GetType(sm.GetTypeInfo(ThisIdentifier, ct).Type.ToFullyQualifiedName, False, True))
      End Function

      <Extension>
      Public Function CalledOnType(n As MemberAccessExpressionSyntax, sm As SemanticModel, ct As CancellationToken) As INamedTypeSymbol
        If (n Is Nothing) OrElse (sm Is Nothing) Then Return Nothing
        Dim s = sm.GetSymbolInfo(n, ct).Symbol
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


