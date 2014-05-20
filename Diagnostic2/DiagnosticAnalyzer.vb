Imports System.Collections.Immutable
Imports System.Runtime.Serialization
Imports Microsoft.CodeAnalysis.Diagnostics

<DiagnosticAnalyzer>
<ExportDiagnosticAnalyzer(DiagnosticAnalyzer.DiagnosticId, LanguageNames.VisualBasic)>
Public Class DiagnosticAnalyzer
  ' TODO: Consider implementing other interfaces that implement IDiagnosticAnalyzer instead of or in addition to ISymbolAnalyzer
  Implements ISyntaxNodeAnalyzer(Of Microsoft.CodeAnalysis.VisualBasic.SyntaxKind)

  Friend Const DiagnosticId = "String.Format Checker"
  Friend Const Description = "Is the string of the String.Fornat valid?"
  Friend Const MessageFormat = "Invalid String for String.Format. (Reason: {0})"
  Friend Const Category = "Validation"

  Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Description, MessageFormat, Category, DiagnosticSeverity.Error)

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

  Public Sub AnalyzeNode(node As SyntaxNode, semanticModel As SemanticModel, addDiagnostic As Action(Of Diagnostic), cancellationToken As CancellationToken) Implements ISyntaxNodeAnalyzer(Of SyntaxKind).AnalyzeNode

    Dim x = CType(node, VisualBasic.Syntax.MemberAccessExpressionSyntax)
    If x Is Nothing Then Exit Sub
    Dim s = x.ToString
    If s.ToLower = "string.format" Then
       
       Dim p = CType(x.Parent, InvocationExpressionSyntax )
       Dim args = p.ArgumentList.Arguments 
       Select Case args.Count
           Case 0 ' Error
          Case Else
            Dim fs = args.First
            Try
              AnalyseFormatString(Nothing,fs.ToString, Enumerable.Repeat(Of Object)(Nothing, args.Count-1).ToArray   )
            Catch ex As ArgIndexOutOfRange
              Dim cex = DirectCast(ex,ArgIndexOutOfRange)
              Dim p0= fs.SpanStart + cex.From
              Dim p1 = fs.SpanStart + 1 + cex.To
              addDiagnostic(Diagnostic.Create(Rule,Location.Create(node.SyntaxTree,TextSpan.FromBounds(p0,p1)),ex.Message ) )
            Catch ex As UnexpectedChar
              Dim cex = DirectCast(ex, UnexpectedChar )
              Dim p0 = fs.SpanStart + cex.Pos
            addDiagnostic(Diagnostic.Create(Rule, Location.Create(node.SyntaxTree, TextSpan.FromBounds(p0, p0 + 1)), "Unexpected Character"))
          Catch ex As UnexpectedlyReachedEndOfText_Exception 
            Dim cex = DirectCast(ex, UnexpectedlyReachedEndOfText_Exception)
            addDiagnostic(Diagnostic.Create(Rule, Location.Create(node.SyntaxTree,fs.Span ), cex.Message ))
            'Debugger.Break 
          End Try
       End Select
      'Debugger.Break()

    End If
  End Sub



  Sub AnalyseFormatString(provider As IFormatProvider, format As String, ParamArray Args() As Object)
    If format Is Nothing Then Throw New ArgumentNullException("fs")
    If Args Is Nothing Then Throw New ArgumentNullException("Args")
    '
    ' This is based on the .net framework implementation of String.Format.
    '

    Const Opening_Brace As Char = "{"c
    Const Closing_Brace As Char = "}"c
    Const _SPACE_ As Char = " "c
    Const _COMMA_ As Char = ","c
    Const _COLON_ As Char = ":"c
    Const _MINUS_ As Char = "-"c
    ' 
    ' Digit::= '0' - '9'
    ' Spaces::= ' '*
    ' IndexPart::= Spaces Digit Spaces
    ' AlignmentPart::= _Comma_ Spaces _MINUS_? Digit* Spaces
    ' FormatPart::= _COLON_ Spaces ?? Spaces
    ' FormatString ::= Opening_Brace IndexPart AlignmentPart? FormatPart? Closing_Brace 
    '
    '
    '
    Const Limit As Integer = 1_000_000


    Dim pos = 0
    Dim len = format.Length
    Dim Curr As Char = ControlChars.NullChar

    Dim cf As ICustomFormatter = Nothing
    If provider IsNot Nothing Then cf = CType(provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)

    While True
      Dim p = pos
      Dim i = pos

      Dim ob = 0
      Dim cb = 0

      While p < len
        Curr = format(pos)
        pos += 1
        Select Case Curr
          Case Closing_Brace
            If (pos < len) AndAlso format(pos) = Closing_Brace Then
              ' This brace has escaped! }}
              pos += 1
            Else
              If (pos >= len) Then Throw New UnexpectedlyReachedEndOfText_Exception()
              Throw New UnexpectedChar(Curr, pos)
            End If
          Case Opening_Brace
            If (pos < len) AndAlso format(pos) = Opening_Brace Then
              ' This brace has escaped! {{
              pos += 1
            Else
              If (pos >= len) Then Throw New UnexpectedlyReachedEndOfText_Exception()
              ob = pos ' This is the char index of the first character in the IndexPart
              pos -= 1

              Exit While
              ' Throw New UnexpectedChar(Curr, pos)
            End If
        End Select
        ' Normally here we would Append( Curr ) but this is just checking the validity of the formatstring.

      End While
      ' Have we reached the end of the format string?
      If (pos = len) Then Exit While
      pos += 1
      If (pos = len) Then Throw New UnexpectedlyReachedEndOfText_Exception()
      Curr = format(pos)
      If Not IsDigit(Curr) Then Throw New UnexpectedChar(Curr, pos)
      ' -- Parse and Calculate IndexPart Value
      Dim Index = 0
      Do
        Index = (10 * Index) + DigitValue(Curr)
        pos += 1
        If pos = len Then Throw New UnexpectedlyReachedEndOfText_Exception
        Curr = format(pos)
      Loop While IsDigit(Curr) AndAlso (Index < Limit)
      ' Why did we exit?
      If Index >= Limit Then
        ' Index Value is greater or equal to limit.
        ' ToDo: Contine the parse characters to the closing brace (or AlignmentPart or FormatPart). So we can underline it in the IDE.
        Throw New FormatException("Index Value has exceeded the limit")
      End If
      cb = pos - 1
      If Index >= Args.Length Then
        ' Index is out of the bounds of the supplied args. 
        Throw New ArgIndexOutOfRange(ob,cb,String.Format("Index of ({0}) is invalid. (0 <= Index < {1})", Index, Args.Length))
        ' ToDo: Get the Start and End positions of opening and closing braces.
      End If
      ' Consume Spaces
      While (pos < len)
        Curr = format(pos)
        If Curr <> _SPACE_ Then Exit While
        pos += 1
      End While

      Dim LeftJustifiy = False
      Dim Width = 0
      ' -- Check for AlignmentPart --
      If Curr = _COMMA_ Then
        pos += 1
        ' Consume spaces
        While (pos < len)
          Curr = format(pos)
          If Curr <> _SPACE_ Then Exit While
          pos += 1
        End While
        If pos = len Then Throw New UnexpectedlyReachedEndOfText_Exception
        Curr = format(pos)
        If Curr = _MINUS_ Then
          LeftJustifiy = True
          pos += 1
          If pos = len Then Throw New UnexpectedlyReachedEndOfText_Exception
          Curr = format(pos)
        End If
        If Not IsDigit(Curr) Then Throw New UnexpectedChar(Curr, pos)

        Do
          Width = (10 * Width) + DigitValue(Curr)
          If pos = len Then Throw New UnexpectedlyReachedEndOfText_Exception
          Curr = format(pos)
        Loop While IsDigit(Curr) AndAlso (Width < Limit)
        ' Why did we exit?
        If Width >= Limit Then
          ' Index Value is greater or equal to limit.
          ' ToDo: Contine the parse characters to the closing brace (or AlignmentPart or FormatPart). So we can underline it in the IDE.
          Throw New FormatException("Alignment Value has exceeded the limit")
        End If

      End If

      ' Consume spaces
      While (pos < len)
        Curr = format(pos)
        If Curr <> _SPACE_ Then Exit While
        pos += 1
      End While

      Dim arg As Object = Args(Index)
      Dim fmt As Text.StringBuilder = Nothing

      ' -- Check for formatting strings --
      If Curr = _COLON_ Then
        pos += 1
        i = pos

        While True
          If pos = len Then Throw New UnexpectedlyReachedEndOfText_Exception
          Curr = format(pos)

          Select Case Curr

            Case Opening_Brace
              If (pos < len) AndAlso format(pos) = Opening_Brace Then
                ' This brace has escaped! {{
                pos += 1
              Else
                If (pos >= len) Then Throw New UnexpectedlyReachedEndOfText_Exception()
                Throw New UnexpectedChar(Curr, pos)
              End If

            Case Closing_Brace
              If (pos < len) AndAlso format(pos) = Closing_Brace Then
                ' This brace has escaped! }}
                pos += 1
              Else
                If (pos >= len) Then Throw New UnexpectedlyReachedEndOfText_Exception()
                pos -= 1
                Exit While
                ' Throw New UnexpectedChar(Curr, pos) 
              End If
          End Select

          If fmt Is Nothing Then fmt = New Text.StringBuilder()
          fmt.Append(Curr)

        End While
      End If

      If Curr <> Closing_Brace Then Throw New UnexpectedChar(Curr, pos)
      pos += 1
      Dim sFmt As String = Nothing
      Dim s As String = Nothing
      If cf IsNot Nothing Then
        If fmt IsNot Nothing Then sFmt = fmt.ToString
        s = cf.Format(sFmt, arg, provider)
      End If

      If s Is Nothing Then
        Dim formattableArg As IFormattable = CType(arg, IFormattable)
        '
        '         #If FEATURE_LEGACYNETCF
        ' If CompatibilitySwitch.IsAppEarlierThanWindows8 Then
        ' // TimeSpan does not implement IFormattable in Mango
        ' If TypeOf arg Is TimeSpan Then formattableArg = null
        ' End If
        ' #End If

        If formattableArg IsNot Nothing Then
          If (sFmt Is Nothing) AndAlso (fmt IsNot Nothing) Then sFmt = fmt.ToString()
          s = formattableArg.ToString(sFmt, provider)
        ElseIf arg IsNot Nothing Then
          s = arg.ToString
        End If

      End If
      '' apply the alignment
      'If s Is Nothing Then s= String.Empty
      'Dim pad = Width - s.Length
      'If (Not LeftJustifiy) AndAlso (pad > 0) Then Append(_SPACE_, pad)
      'Append(s)
      'If LeftJustifiy AndAlso (pad > 0) Then Append(_SPACE_,pad) 
      ''


    End While
    'Return me


  End Sub


  Private Function IsDigit(c As Char) As Boolean
    Return c >= "0"c AndAlso c <= "9"c
  End Function

  Private Function DigitValue(c As Char) As Integer
    Select Case c
      Case "0"c : Return 0
      Case "1"c : Return 1
      Case "2"c : Return 2
      Case "3"c : Return 3
      Case "4"c : Return 4
      Case "5"c : Return 5
      Case "6"c : Return 6
      Case "7"c : Return 7
      Case "8"c : Return 8
      Case "9"c : Return 9
      Case Else
        Return 0
    End Select
  End Function

  <Serializable>
  Private Class UnexpectedlyReachedEndOfText_Exception
    Inherits Exception

    Public Sub New()
    End Sub

    Public Sub New(message As String)
      MyBase.New(message)
    End Sub

    Public Sub New(message As String, innerException As Exception)
      MyBase.New(message, innerException)
    End Sub

    Protected Sub New(info As SerializationInfo, context As StreamingContext)
      MyBase.New(info, context)
    End Sub
  End Class

  <Serializable>
  Private Class UnexpectedChar
    Inherits Exception

    Private _pos As Integer = -1
    Public ReadOnly Property Pos As Integer
      Get
        Return _pos
      End Get
    End Property

    Private _C As Char
    Public ReadOnly Property C As Char
      Get
        Return _C
      End Get
    End Property

    Public Sub New()
    End Sub

    Public Sub New(message As String)
      MyBase.New(message)
    End Sub

    Public Sub New(c As Char, pos As Integer)
      MyBase.New()
      _C = c
      _pos = pos
    End Sub

    Public Sub New(message As String, innerException As Exception)
      MyBase.New(message, innerException)
    End Sub

    Protected Sub New(info As SerializationInfo, context As StreamingContext)
      MyBase.New(info, context)
    End Sub
  End Class

  <Serializable>
  Private Class ArgIndexOutOfRange
    Inherits Exception

    Private _OB As Integer = -1
    Public ReadOnly Property [From] As Integer
      Get
        Return _OB
      End Get
    End Property

    Private _CB As Integer =-1.0R
    Public ReadOnly Property [To] As Integer
      Get
        Return _CB
      End Get
    End Property

    Public Sub New()
    End Sub

    Public Sub New(message As String)
      MyBase.New(message)
    End Sub

    Public Sub New(OB As Integer, CB As Integer,msg As string)
      MyBase.New(msg)
      _CB = CB
      _OB = OB
    End Sub

    Public Sub New(message As String, innerException As Exception)
      MyBase.New(message, innerException)
    End Sub

    Protected Sub New(info As SerializationInfo, context As StreamingContext)
      MyBase.New(info, context)
    End Sub
  End Class
End Class