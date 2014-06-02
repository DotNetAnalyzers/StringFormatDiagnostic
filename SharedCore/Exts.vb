Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft

Namespace Global.Roslyn.StringFormatDiagnostics
  Namespace VisualBasic
  Public Module Exts

      <Extension>
      Public Function ArgumentType(arg As CodeAnalysis.VisualBasic.Syntax.ArgumentSyntax, sm As SemanticModel, ct As CancellationToken) As ITypeSymbol
        Return sm.GetTypeInfo(CType(arg, CodeAnalysis.VisualBasic.Syntax.SimpleArgumentSyntax).Expression, ct).Type
      End Function

      <Extension>
    Public Function GetArgumentTypes(args As CodeAnalysis.VisualBasic.Syntax.ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of ITypeSymbol)
      Return args.Arguments.Select(Function(arg) arg.ArgumentType(sm, ct))
    End Function

    <Extension>
    Public Function GetArgumentTypesNames(args As CodeAnalysis.VisualBasic.Syntax.ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of String)
      Return args.GetArgumentTypes(sm, ct).Select(Function(tsym) tsym.ToFullyQualifiedName)
    End Function

    <Extension>
    Public Iterator Function GetArgumentAsObjects(args As CodeAnalysis.VisualBasic.Syntax.ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of Object)
      Dim ArgTypes = args.GetArgumentTypes(sm, ct)
      For i = 0 To args.Arguments.Count - 1
        Dim FullyNamed = ArgTypes(i).ToFullyQualifiedName
        Dim GottenType = Type.GetType(FullyNamed, False, True)
        Dim ov As Object
        Try
          ov = Convert.ChangeType(args.Arguments(i).ToString, GottenType)
        Catch ex As Exception
          ov = New Object
        End Try
        Yield ov
      Next
    End Function


    <Extension>
    Public Function CalledOnType(n As CodeAnalysis.VisualBasic.Syntax.MemberAccessExpressionSyntax, sm As SemanticModel, ct As CancellationToken) As INamedTypeSymbol
      Dim s = sm.GetSymbolInfo(n, ct).Symbol
      Return If(s Is Nothing, Nothing, s.ContainingType)
    End Function

    <Extension>
    Public Function ToFullyQualifiedName(s As ISymbol) As String
      Return s.ToDisplayString(New SymbolDisplayFormat(typeQualificationStyle:=SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces))
    End Function

    '<Extension>
    'Public Function BuildMeOne(tt As Type) As Object
    '  Dim constructors = tt.GetConstructors()
    '  Dim a() As Object
    '  For Each xon In constructors
    '    Try
    '      ReDim a(xon.GetParameters.Count())
    '      Dim obj = xon.Invoke(a)
    '      Return obj
    '    Catch ex As Exception

    '    End Try
    '  Next
    '  Return Nothing ' Throw New Exception
    'End Function
  End Module

    End Namespace
  Namespace CSharp
    Public Module Exts

      <Extension>
      Public Function ArgumentType(arg As CodeAnalysis.CSharp.Syntax.ArgumentSyntax, sm As SemanticModel, ct As CancellationToken) As ITypeSymbol
        Return sm.GetTypeInfo(CType(arg, CodeAnalysis.CSharp.Syntax.ArgumentSyntax).Expression, ct).Type
      End Function

      <Extension>
      Public Function GetArgumentTypes(args As CodeAnalysis.CSharp.Syntax.ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of ITypeSymbol)
        Return args.Arguments.Select(Function(arg) arg.ArgumentType(sm, ct))
      End Function

      <Extension>
      Public Function GetArgumentTypesNames(args As CodeAnalysis.CSharp.Syntax.ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of String)
        Return args.GetArgumentTypes(sm, ct).Select(Function(tsym) tsym.ToFullyQualifiedName)
      End Function

      <Extension>
      Public Iterator Function GetArgumentAsObjects(args As CodeAnalysis.CSharp.Syntax.ArgumentListSyntax, sm As SemanticModel, ct As CancellationToken) As IEnumerable(Of Object)
        Dim ArgTypes = args.GetArgumentTypes(sm, ct)
        For i = 0 To args.Arguments.Count - 1
          Dim FullyNamed = ArgTypes(i).ToFullyQualifiedName
          Dim GottenType = Type.GetType(FullyNamed, False, True)
          Dim ov As Object
          Try
            ov = Convert.ChangeType(args.Arguments(i).ToString, GottenType)
          Catch ex As Exception
            ov = New Object
          End Try
          Yield ov
        Next
      End Function


      <Extension>
      Public Function CalledOnType(n As CodeAnalysis.CSharp.Syntax.MemberAccessExpressionSyntax, sm As SemanticModel, ct As CancellationToken) As INamedTypeSymbol
        Dim s = sm.GetSymbolInfo(n, ct).Symbol
        Return If(s Is Nothing, Nothing, s.ContainingType)
      End Function

      <Extension>
      Public Function ToFullyQualifiedName(s As ISymbol) As String
        Return s.ToDisplayString(New SymbolDisplayFormat(typeQualificationStyle:=SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces))
      End Function
      End Module

  End Namespace
End Namespace
