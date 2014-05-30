Imports System.Runtime.CompilerServices

Public Module Exts

  <Extension>
  Public Function ArgumentType(arg As ArgumentSyntax, sm As SemanticModel, ct As CancellationToken) As ITypeSymbol
    Return sm.GetTypeInfo(CType(arg, SimpleArgumentSyntax).Expression, ct).Type
  End Function

  <Extension>
  Public Function GetArgumentTypes(args As ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of ITypeSymbol)
    Return args.Arguments.Select(Function(arg) arg.ArgumentType(sm, ct))
  End Function

  <Extension>
  Public Function GetArgumentTypesNames(args As ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of String)
    Return args.GetArgumentTypes(sm, ct).Select(Function(tsym) tsym.ToFullyQualifiedName)
  End Function

  <Extension>
  Public Function CalledOnType(n As MemberAccessExpressionSyntax, sm As SemanticModel, ct As CancellationToken) As INamedTypeSymbol
    Dim s = sm.GetSymbolInfo(n, ct).Symbol
    Return If(s Is Nothing, Nothing, s.ContainingType)
  End Function

  <Extension>
  Public Function ToFullyQualifiedName(s As ISymbol) As String
    Return s.ToDisplayString(New SymbolDisplayFormat(typeQualificationStyle:=SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces))
  End Function

  <Extension>
  Public Function BuildMeOne(tt As Type) As Object
    Dim constructors = tt.GetConstructors()
    Dim a() As Object
    For Each xon In constructors
      Try
        ReDim a(xon.GetParameters.Count())
        Dim obj = xon.Invoke(a) 
        Return obj
      Catch ex As Exception

      End Try
    Next
   Return Nothing ' Throw New Exception
  End Function
End Module