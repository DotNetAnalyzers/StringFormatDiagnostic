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
        If args Is Nothing Then Return Enumerable.Empty(Of ITypeSymbol)
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
                    Dim ov As Object
                    Dim Arg = CType(args.Arguments(i), CodeAnalysis.VisualBasic.Syntax.SimpleArgumentSyntax)
                    If TypeOf Arg.Expression Is CodeAnalysis.VisualBasic.Syntax.IdentifierNameSyntax Then
                        ov = IdentifierValue(DirectCast( Arg.Expression,IdentifierNameSyntax), sm, ct)
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
        Return sm.GetSymbolInfo(sn, ct).Symbol.IsExtern
      End Function

      <Extension>
            Function IdentifierValue(ThisIdentifier As CodeAnalysis.VisualBasic.Syntax.IdentifierNameSyntax, sm As SemanticModel, ct As CancellationToken) As Object
                If ThisIdentifier Is Nothing Then Return Nothing
                'Dim ConstValue = sm.GetConstantValue(ThisIdentifier, ct)
                ''If ConstValue.HasValue = False Then  return nothing
                Dim FoundSymbol = sm.LookupSymbols(ThisIdentifier.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
                Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, CodeAnalysis.VisualBasic.Syntax.VariableDeclaratorSyntax)
                If VariableDeclarationSite Is Nothing Then Return Nothing
                Dim TheValueOfTheVariable = VariableDeclarationSite.Initializer.Value.DescendantTokens().First.Value
                Return Convert.ChangeType(TheValueOfTheVariable, Type.GetType(sm.GetTypeInfo(ThisIdentifier, ct).Type.ToFullyQualifiedName, False, True))

            End Function


            '      Function PotentiallyChanged(sn As SyntaxNode,sm As SemanticModel, ct As CancellationToken ) As Boolean
            'Dim d = sm.GetSymbolInfo(sn,ct).Symbol.DeclaringSyntaxReferences(0).GetSyntax.Parent        
            ' Dim v = sm.AnalyzeDataFlow(d,sn)
            ' Dim wo =      v.WrittenOutside ()
            ' Return        wo(0).
            '      End Function


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
                        ov = Convert.ChangeType(args.Arguments(i).GetFirstToken.ValueText, GottenType)
                    Catch ex As Exception
                        ov = Nothing
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
