Option Strict On
Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text

Namespace Global.Roslyn.StringFormatDiagnostics

  Public Module Common


    Public Const DiagnosticId = "FormatString Diagnostic"
    Public Const Description = "Is the formatstring valid?"
    Public Const MessageFormat = "Invalid FormatString (Reason: {0})"
    Public Const Category = "Validation"
    Public Rule1 As New DiagnosticDescriptor(DiagnosticId, Description, MessageFormat, Category, DiagnosticSeverity.Error)
    Public Rule2 As New DiagnosticDescriptor(DiagnosticId, Description, "This Constant is used as a FormatString" + Environment.NewLine + MessageFormat, Category, DiagnosticSeverity.Error)


    Public Function AddWarning(node As SyntaxNode, offset As Integer, endoffset As Integer, ri As IssueReport) As Diagnostic
      Return Diagnostic.Create(Rule1, Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), ri.Message)
    End Function


    Public Function AddWarningAtSource(node As SyntaxNode, offset As Integer, endoffset As Integer, ri As IssueReport) As Diagnostic
      Return Diagnostic.Create(Rule2, Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), ri.Message)
    End Function


    Public Function AddInformation(node As SyntaxNode, msg As String) As Diagnostic
      Return Diagnostic.Create(DiagnosticId, Category, msg, DiagnosticSeverity.Info, 0, False, Location.Create(node.SyntaxTree, node.Span))
    End Function


    Const Opening_Brace As Char = "{"c
    Const Closing_Brace As Char = "}"c
    Const _SPACE_ As Char = " "c
    Const _COMMA_ As Char = ","c
    Const _COLON_ As Char = ":"c
    Const _MINUS_ As Char = "-"c
    Const _LIMIT_ As Integer = 1_000_000  ' This limit is found inside the .net implementation of String.Format.
    Const ExitOnFirst = False



    Iterator Function AnalyseFormatString(ct As CancellationToken, format As String, NumOfArgs As Integer,
                                           Args As IEnumerable(Of Object),
                                          Optional Provider As IFormatProvider = Nothing) As IEnumerable(Of IssueReport)
      If format Is Nothing Then Throw New ArgumentNullException("fs")
      'If Args Is Nothing Then Throw New ArgumentNullException("Args")
      '
      ' This is based on the .net framework implementation of String.Format.
      '
      ' Rough Grammar Rules
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

      Dim fr As StringReader = StringReader.Create(format)

      Dim output As New System.Text.StringBuilder
      Dim ArgsSupplied = NumOfArgs > 0
      Dim ArgsCounted = 0
      Dim internalError As Internal_IssueReport = Nothing
      'Dim CurrentCharacter = ControlChars.NullChar
      Dim Width = 0
      Dim cf As ICustomFormatter = Nothing
      If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      Try
        While True
          Dim InvalidIndex As Boolean = False
          Dim ParsingIsInAnErrorState = False ' This flag enables the parser to continue parsing whilst there is an issue found. Allowing us to report additional issue.
          Dim StartPositionForThisPart = 0
          Dim EndPositionForThisPart = 0

          While fr.IsNotBeyondEndOfText

            Dim ThisChar = fr.Curr.Value

            Select Case ThisChar
              Case Closing_Brace
                If fr.IsNotBeyondEndOfText AndAlso fr.Peek.Value = Closing_Brace Then
                  ' This brace has escaped! }}
                  fr.Next()
                Else
                  If ArgsSupplied AndAlso fr.IsBeyondEndOfText Then
                    Yield New UnexpectedlyReachedEndOfText
                    Exit Function
                  End If
                  Yield New UnexpectedChar(ThisChar, fr.Index)
                  If ExitOnFirst Then Exit Function
                End If
              Case Opening_Brace
                If fr.IsNotBeyondEndOfText AndAlso fr.Peek = Opening_Brace Then
                  ' This brace has escaped! {{
                  fr.Next()
                Else
                  If ArgsSupplied AndAlso fr.IsBeyondEndOfText Then
                    Yield New UnexpectedlyReachedEndOfText
                    Exit Function
                  End If
                  StartPositionForThisPart = fr.Index + 1  ' This is the char index of the first character in the IndexPart

                  Exit While

                End If
            End Select

            'Normally here we would Append( Curr ) but this is just checking the validity of the formatstring.
            output.Append(ThisChar)
            fr.Next()
            If ct.IsCancellationRequested Then Exit Function
          End While
          ' Have we reached the end of the format string?
          If fr.IsBeyondEndOfText Then Exit While
          fr.Next()
          If ArgsSupplied AndAlso fr.IsBeyondEndOfText Then Yield New UnexpectedlyReachedEndOfText : Exit Function
          ' Get current character of the text.
          'CurrentCharacter = fr.Curr.Value
          If ArgsSupplied AndAlso Not IsDigit(fr.Curr.Value) Then
            ParsingIsInAnErrorState = True
            Yield New UnexpectedChar(fr.Curr.Value, fr.Index)
            If ExitOnFirst Then Exit Function
          End If
          '
          ' +---------------------------------------------------
          ' | Start Parsing for the Index value
          ' +---------------------------------------------------
          '
          ' -- Parse and Calculate IndexPart Value
          Dim ArgIndex = ParseValue(fr,ct,_Limit_,ParsingIsInAnErrorState)
          'Dim ArgIndex = 0
          'Do
          '  ' Was cancellation requestes? If so exit.
          '  If ct.IsCancellationRequested Then Exit Function
          '  If Not ParsingIsInAnErrorState Then
          '    ' Work out the new value of the Index
          '    ArgIndex = (10 * ArgIndex) + DigitValue(fr.Curr.Value)
          '  End If
          '  fr.Next()
          '  If ArgsSupplied AndAlso fr.IsBeyondEndOfText Then Yield New UnexpectedlyReachedEndOfText : Exit Function
          '  'CurrentCharacter = fr.Curr.Value
          '  If Not ParsingIsInAnErrorState AndAlso (ArgIndex >= _LIMIT_) Then ParsingIsInAnErrorState = True
          'Loop While IsDigit(fr.Curr.Value)
          ' Why did we exit?
          ArgsCounted += 1
          EndPositionForThisPart = fr.Index - 1
          If ArgsSupplied AndAlso ArgIndex >= _LIMIT_ Then
            ' Index Value is greater or equal to limit.
            Yield New ArgIndexHasExceedLimit("Arg Index", ArgIndex.ToString, _LIMIT_, StartPositionForThisPart, EndPositionForThisPart)
            InvalidIndex = True
            If ExitOnFirst Then Exit Function
          End If
          If ArgsSupplied AndAlso Not ParsingIsInAnErrorState AndAlso (ArgIndex >= NumOfArgs) Then
            ' Index is out of the bounds of the supplied args.
            Yield New ArgIndexOutOfRange(ArgIndex, NumOfArgs, StartPositionForThisPart, EndPositionForThisPart)
            ' ToDo: Get the Start and End positions of opening and closing braces.
            InvalidIndex = True
            If ExitOnFirst Then Exit Function
          End If
          ' Reset the ParsingIsInAnErrorState Flag 
          ParsingIsInAnErrorState = False
          ConsumeSpaces(fr, ct)
          If fr.IsBeyondEndOfText Then Yield New UnexpectedlyReachedEndOfText : Exit Function
          '
          ' +-----------------------------------------
          ' | Start of Parsing for the AlignmentPart  
          ' +-----------------------------------------
          '
          Dim LeftJustifiy = False
          If fr.Curr.Value = _COMMA_ Then
            fr.Next()
            ConsumeSpaces(fr, ct)
            If ArgsSupplied AndAlso fr.IsBeyondEndOfText Then Yield New UnexpectedlyReachedEndOfText : Exit Function
            'CurrentCharacter = fr.Curr.Value
            If fr.Curr.Value = _MINUS_ Then
              LeftJustifiy = True
              fr.Next()
              If ArgsSupplied AndAlso fr.IsBeyondEndOfText Then Yield New UnexpectedlyReachedEndOfText : Exit Function
              'CurrentCharacter = fr.Curr.Value
            End If
            ' If next Character after a minus, or currenct character isnot a Digit then it is an error
            If ArgsSupplied AndAlso Not IsDigit(fr.Curr.Value) Then
              ParsingIsInAnErrorState = True
              Yield New UnexpectedChar(fr.Curr.Value, fr.Index)
              If ExitOnFirst Then Exit Function
            End If
            ' Reset the markers for highlighter
            StartPositionForThisPart = fr.Index
            EndPositionForThisPart = fr.Index
            Width = ParseValue(fr,ct,_LIMIT_,ParsingIsInAnErrorState )
            'Width = 0
            'Do
            '  If ct.IsCancellationRequested Then Exit Function
            '  If Not ParsingIsInAnErrorState Then
            '    ' Calculate the new value of width
            '    Width = (10 * Width) + DigitValue(fr.Curr.Value)
            '  End If
            '  fr.Next()
            '  If fr.IsBeyondEndOfText Then
            '    Yield New UnexpectedlyReachedEndOfText
            '    Exit Function
            '  End If
            '  If Not ParsingIsInAnErrorState AndAlso (Width >= _LIMIT_) Then ParsingIsInAnErrorState = True
            '  'CurrentCharacter = fr.Curr.Value
            'Loop While IsDigit(fr.Curr.Value) 'AndAlso (Width < _LIMIT_)
            ' Why did we exit?
            EndPositionForThisPart = fr.Index - 1
            If ArgsSupplied AndAlso Width >= _LIMIT_ Then
              ' Index Value is greater or equal to limit.
              Dim WidthText = format.Substring(StartPositionForThisPart, (EndPositionForThisPart - StartPositionForThisPart) + 1)
              Yield New ArgIndexHasExceedLimit("Value when limit was exceeded. ", WidthText, _LIMIT_, StartPositionForThisPart, EndPositionForThisPart)
              If ExitOnFirst Then Exit Function
            End If
          End If
          ConsumeSpaces(fr, ct)
          If ArgsSupplied AndAlso Not ParsingIsInAnErrorState AndAlso fr.IsBeyondEndOfText Then Yield New UnexpectedlyReachedEndOfText : Exit Function
          '
          ' +--------------------------------------------
          ' |  Start of Parsing for formatting strings 
          ' +--------------------------------------------
          '
          Dim fmt As System.Text.StringBuilder = Nothing
          If fr.Curr.Value = _COLON_ Then
            fr.Next()
            While True
              If ct.IsCancellationRequested Then Exit Function
              If ArgsSupplied AndAlso fr.IsBeyondEndOfText Then Yield New UnexpectedlyReachedEndOfText : Exit Function
              Dim ThisChar = fr.Curr.Value
              Select Case ThisChar
                Case Opening_Brace
                  If fr.IsNotBeyondEndOfText AndAlso fr.Peek.Value = Opening_Brace Then
                    ' This brace has escaped! {{
                    fr.Next()
                  Else
                    If ArgsSupplied AndAlso fr.IsBeyondEndOfText Then Yield New UnexpectedlyReachedEndOfText : Exit Function
                    If ArgsSupplied Then Yield New UnexpectedChar(ThisChar, fr.Index)
                    If ExitOnFirst Then Exit Function
                  End If
                Case Closing_Brace
                  If fr.IsNotBeyondEndOfText AndAlso fr.Peek.Value = Closing_Brace Then
                    ' This brace has escaped! }}
                    fr.Next()
                  Else
                    If ArgsSupplied AndAlso fr.IsBeyondEndOfText Then Yield New UnexpectedlyReachedEndOfText : Exit Function
                    Exit While
                  End If

              End Select
              If fmt Is Nothing Then fmt = New System.Text.StringBuilder
              fmt.Append(ThisChar)
              fr.Next()
            End While
          End If
          If fr.Curr.Value <> Closing_Brace Then
            If ArgsSupplied Then
              ParsingIsInAnErrorState = True
              Yield New UnexpectedChar(fr.Curr.Value, fr.Index)
            End If
            If ExitOnFirst Then Exit Function
          End If
          fr.Next()
          '
          ' NOTE: Probably don't need to following for just checking the validation
          '
          Dim sFmt As String = Nothing
          Dim s As String = Nothing
          Dim arg = If(InvalidIndex = True, "", Args(ArgIndex))
          If cf IsNot Nothing Then
            If fmt IsNot Nothing Then sFmt = fmt.ToString
            s = cf.Format(sFmt, arg, Provider)
          End If

          If s Is Nothing Then
            Dim formattableArg As IFormattable = TryCast(arg, IFormattable)
            '
            '         #If FEATURE_LEGACYNETCF
            ' If CompatibilitySwitch.IsAppEarlierThanWindows8 Then
            ' // TimeSpan does not implement IFormattable in Mango
            ' If TypeOf arg Is TimeSpan Then formattableArg = null
            ' End If
            ' #End If

            If formattableArg IsNot Nothing Then
              If (sFmt Is Nothing) AndAlso (fmt IsNot Nothing) Then sFmt = fmt.ToString()
              s = formattableArg.ToString(sFmt, Provider)
            ElseIf arg IsNot Nothing Then
              s = arg.ToString
            End If

          End If

          ' apply the alignment
          If s Is Nothing Then s = String.Empty
          Dim pad = Width - s.Length
          If (Not LeftJustifiy) AndAlso (pad > 0) Then output.Append(_SPACE_, pad)
          output.Append(s)
          If LeftJustifiy AndAlso (pad > 0) Then output.Append(_SPACE_, pad)
          '
        End While
        If ArgsSupplied Then
          If ArgsCounted = 0 Then Yield New ContainsNoArgs
        Else
          If ArgsCounted > 0 Then Yield New ContainsNoParameters
        End If
      Catch ex As Exception
        ' Let's use the IDE error window to also report internal errors, :-)
        internalError = New Internal_IssueReport(ex.ToString)
      End Try
      If internalError IsNot Nothing Then Yield internalError
      Yield New FinalOutput(output.ToString)
    End Function

    Private Function ParseValue(fr As StringReader, ct As CancellationToken, Limit As Integer, ByRef ParsingIsInAnErrorState As Boolean) As Integer 
     Dim Value = 0
      Do
        If ct.IsCancellationRequested Then Return Value
        If Not ParsingIsInAnErrorState Then Value = (10 * Value) + DigitValue(fr.Curr.Value)
        fr.Next()
        If fr.IsBeyondEndOfText Then Return Value
        If Not ParsingIsInAnErrorState AndAlso Value >= Limit Then ParsingIsInAnErrorState = True
      Loop While IsDigit(fr.Curr.Value )
      Return Value 
    End Function

    Private Sub ConsumeSpaces(byref fr As StringReader, ct As CancellationToken)
      ' Consume spaces
      While fr.IsNotBeyondEndOfText
        If ct.IsCancellationRequested Then Exit Sub
        If fr.Curr.Value <> _SPACE_ Then Exit While
        fr.Next()
      End While
    End Sub

    Private Function IsDigit(c As Char) As Boolean
      Return "0"c <= c AndAlso c <= "9"c
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

  End Module


  Public Class UnexpectedlyReachedEndOfText
    Inherits IssueReport
    Public Sub New()
      MyBase.New("Unexpectedly Reached End Of Text")
    End Sub
  End Class


  Public Class ArgIndexHasExceedLimit
    Inherits IssueReportWithStartPosition
    Public ReadOnly Property Finish As Integer
    Public Sub New(ParamName As String, Value As String, Limit As Integer, start As Integer, Finish As Integer)
      MyBase.New(String.Format("{2} of ({0}) has exceed .net String.Format limit of {1}.", Value, Limit, ParamName), start)
      _Finish = Finish
    End Sub
  End Class

  Public Class ArgIndexOutOfRange
    Inherits IssueReportWithStartPosition
    Public ReadOnly Property Finish As Integer
    Public Sub New(Index As Integer, Limit As Integer, start As Integer, Finish As Integer)
      MyBase.New(String.Format("Index of ({0}) is invalid. (0 <= Index < {1})", Index, Limit), start)
      _Finish = Finish
    End Sub
  End Class

  Public Class UnexpectedChar
    Inherits IssueReportWithStartPosition
    Public Sub New(C As Char, Start As Integer)
      MyBase.New("Unexpected Character '" & C & "'", Start)
    End Sub
  End Class

  Public Class ContainsNoArgs
    Inherits IssueReport
    Public Sub New()
      MyBase.New("")
    End Sub
  End Class

  Public Class ContainsNoParameters
    Inherits IssueReport
    Public Sub New()
      MyBase.New("")
    End Sub
  End Class

  Public Class FinalOutput
    Inherits IssueReport
    Public Sub New(output As String)
      MyBase.New(String.Format("Output:= {0}", output))
    End Sub
  End Class

  Public MustInherit Class IssueReportWithStartPosition
    Inherits IssueReport
    Public ReadOnly Property Start As Integer
    Friend Sub New(Msg As String, Start As Integer)
      MyBase.New(Msg)
      _Start = Start
    End Sub
  End Class

  Public Class Internal_IssueReport
    Inherits IssueReport
    Sub New(Msg As String)
      MyBase.New(Msg)
    End Sub
  End Class

  Public MustInherit Class IssueReport
    Public ReadOnly Property Message As String
    Friend Sub New(Msg As String)
      _Message = Msg
    End Sub

  End Class

 Public Class StringReader
    Public ReadOnly Property Source As String
    Public ReadOnly Property Index As Integer

    Public ReadOnly Property Curr As Char?
      Get
        If Index < 0 Then Return New Char?
        If Index >= Source.Length Then Return New Char?
        Return New Char?(Source(Index))
      End Get
    End Property
    Public ReadOnly Property Peek As Char?
      Get
        If Index+1 < 0 Then Return New Char?
        If Index+1 >= Source.Length Then Return New Char?
        Return New Char?(Source(Index+1))
      End Get
    End Property
    Public Sub [Next]()
      If Index = Source.Length Then Exit Sub
      _Index += 1
    End Sub
    Public Sub Back()
      If Index < 0 Then Exit Sub
      _Index -=1
    End Sub

    Public Sub New(s As String)
      _Source = s
      _Index = 0
    End Sub

    Public ReadOnly Property IsBeyondEndOfText() As Boolean
      Get
        Return Index >= Source.Length
      End Get
    End Property
    Public ReadOnly Property IsNotBeyondEndOfText() As Boolean
      Get
        Return Not IsBeyondEndOfText
      End Get
    End Property
    Public Overrides  Function ToString() As String
      Dim s=""
      If Me.Index >= 0 Then s = Me.Source.Substring(0, Index + 1)
      s &= "->" & Me.Curr.Value & "<-"
      If Me.Index < (Me.Source.Length - 1) Then s &= Me.Source.Substring(Me.Index + 1)
      Return s
    End Function
    Public Shared Function Create(Source As String) As StringReader
      If Source Is Nothing Then Return Nothing
      Return New StringReader(Source)
    End Function

  End Class
End Namespace