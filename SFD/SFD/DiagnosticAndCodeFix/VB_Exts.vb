Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft

Namespace Global.AdamSpeight2008.StringFormatDiagnostics
    Namespace VisualBasic
        Public Module Exts

            <Extension>
            Public Function ArgumentType(arg As ArgumentSyntax, sm As SemanticModel, ct As CancellationToken) As ITypeSymbol
                Try
                    Return sm.GetTypeInfo(CType(arg, SimpleArgumentSyntax).Expression, ct).Type
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
                    Dim Arg = CType(args.Arguments(i), SimpleArgumentSyntax)
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
                If (sn Is Nothing) OrElse (sm Is Nothing) Then Return True
                Return sm.GetSymbolInfo(sn, ct).Symbol.IsExtern
            End Function

            <Extension>
            Function IdentifierValue(ThisIdentifier As IdentifierNameSyntax, sm As SemanticModel, ct As CancellationToken) As Object
                If AnyIsNull(Of Object)({ThisIdentifier, sm}) Then Return Nothing

                Dim FoundSymbol = sm.LookupSymbols(ThisIdentifier.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
                Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, CodeAnalysis.VisualBasic.Syntax.VariableDeclaratorSyntax)
                If VariableDeclarationSite Is Nothing Then Return Nothing
                If VariableDeclarationSite.Initializer Is Nothing Then Return Nothing
                If VariableDeclarationSite.Initializer.Value Is Nothing Then Return Nothing

                'If AnyIsNull(Of Object)( {VariableDeclarationSite,VariableDeclarationSite.Initializer,VariableDeclarationSite.Initializer.Value }) THen Return nothing
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





'    <HideModuleName()>
'    Public Module ParsedChar_Exts
'        <Extension>
'        Public Function IsWhitespace(cp As IParsedChar) As Boolean
'            Return (cp IsNot Nothing) AndAlso Char.IsWhiteSpace(cp.Value)
'        End Function

'#If _Define_Alphabetic_ = 0 Then
'    <Extension()>
'    Public Function IsLetter(pc As ParsedChar) As Boolean
'      Return (pc IsNot Nothing) AndAlso Char.IsLetter(pc.Value)
'    End Function
'#Else
'        <Extension()>
'        Public Function IsLetter(pc As IParsedChar) As Boolean
'            Return (pc IsNot Nothing) AndAlso ((pc.Value >= "A"c AndAlso pc.Value <= "Z"c) OrElse (pc.Value >= "a"c AndAlso pc.Value <= "z"c))
'        End Function
'#End If
'        <Extension>
'        Public Function IsEoT(pc As IParsedChar) As Boolean
'            Return pc Is Nothing
'        End Function

'        <Extension>
'        Public Function IsNotEoT(pc As IParsedChar) As Boolean
'            Return pc IsNot Nothing
'        End Function

'        Public Function ParseDigits(pc As IParsedChar) As OutputResult(Of String)
'            Dim curr = pc
'            Dim res As New OutputResult(Of String) With {.Output = ""}
'            While curr.IsNotEoT AndAlso curr.IsDigit
'                res.Output &= curr.Value
'                curr = curr.Next
'            End While
'            Return res.LastParse(curr)
'        End Function

'        <Extension>
'        Public Function RepCount(pc As IParsedChar, c As Char) As OutputResult(Of Integer)
'            Dim res As New OutputResult(Of Integer)
'            Dim curr = pc
'            While curr.IsNotEoT AndAlso (curr.Value = c)
'                res.Output += 1
'            End While
'            Return res.LastParse(curr)
'        End Function

'        <Extension>
'        Public Function IsDigit(c As Char) As Boolean
'            Return Char.IsDigit(c)
'        End Function

'        <Extension>
'        Public Function IsDigit(pc As IParsedChar) As Boolean
'            Return (pc IsNot Nothing) AndAlso ((pc.Value >= "0"c) AndAlso (pc.Value <= "9"c))
'        End Function

'        <Extension>
'        Public Function IsLetterOrWhitespace(pc As IParsedChar) As Boolean
'            Return (pc IsNot Nothing) AndAlso (pc.IsLetter OrElse pc.IsWhitespace)
'        End Function

'        <Extension>
'        Public Function ContainsMoreThan(fs As String, NoMoreThan As Integer, pred As Func(Of Char, Boolean)) As Boolean
'            If AnyIsNull(Of Object)({fs, pred}) Then Return False
'            Dim count, index As Integer
'            While index < fs.Count
'                If pred(fs(index)) Then
'                    count += 1
'                    If count > NoMoreThan Then Return True
'                End If
'                index += 1
'            End While
'            Return False
'        End Function

'    End Module
'End Namespace

