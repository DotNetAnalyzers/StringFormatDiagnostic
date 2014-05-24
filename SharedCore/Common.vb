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
    Public Rule2 As New DiagnosticDescriptor(DiagnosticId, Description, "This Constant is used in either:-
  String.Format
  Console.Write
  Console.WriteLine
 " + MessageFormat, Category, DiagnosticSeverity.Error)
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
    Iterator Function AnalyseFormatString(cancellationToken As CancellationToken, format As String, NumOfArgs As Integer) As IEnumerable(Of IssueReport)
      ' ParamArray Args() As Object) As IEnumerable(Of IssueReport)
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
      Dim ArgsCounted = 0
      Dim internalError As Internal_IssueReport = Nothing
      Dim CurrentPosition = 0
      Dim LengthOfTheText = format.Length
      Dim CurrentCharacter = ControlChars.NullChar
      Dim cf As ICustomFormatter = Nothing
      'If provider IsNot Nothing Then cf = CType(provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      Try
        While True
          Dim StartPositionForThisPart = 0
          Dim EndPositionForThisPart = 0
          While CurrentPosition < LengthOfTheText
            CurrentCharacter = format(CurrentPosition)
            CurrentPosition += 1
            Select Case CurrentCharacter
              Case Closing_Brace
                If (CurrentPosition < LengthOfTheText) AndAlso format(CurrentPosition) = Closing_Brace Then
                  ' This brace has escaped! }}
                  CurrentPosition += 1
                Else
                  If (CurrentPosition >= LengthOfTheText) Then
                    Yield New UnexpectedlyReachedEndOfText
                    Exit Function
                  End If
                  Yield New UnexpectedChar(CurrentCharacter, CurrentPosition)
                  If ExitOnFirst Then Exit Function
                End If
              Case Opening_Brace
                If (CurrentPosition < LengthOfTheText) AndAlso format(CurrentPosition) = Opening_Brace Then
                  ' This brace has escaped! {{
                  CurrentPosition += 1
                Else
                  If (CurrentPosition >= LengthOfTheText) Then
                    Yield New UnexpectedlyReachedEndOfText
                    Exit Function
                  End If
                  StartPositionForThisPart = CurrentPosition ' This is the char index of the first character in the IndexPart
                  CurrentPosition -= 1
                  Exit While
                  ' Throw New UnexpectedChar(Curr, pos)
                End If
            End Select
            ' Normally here we would Append( Curr ) but this is just checking the validity of the formatstring.
            If cancellationToken.IsCancellationRequested Then Exit Function
          End While
          ' Have we reached the end of the format string?
          If (CurrentPosition >= LengthOfTheText) Then Exit While
          CurrentPosition += 1
          If (CurrentPosition >= LengthOfTheText) Then Yield New UnexpectedlyReachedEndOfText : Exit Function
          ' Get current character of the text.
          CurrentCharacter = format(CurrentPosition)
          If Not IsDigit(CurrentCharacter) Then
            Yield New UnexpectedChar(CurrentCharacter, CurrentPosition)
            If ExitOnFirst Then Exit Function
          End If
          '
          ' +---------------------------------------------------
          ' | Start Parsing for the Index value
          ' +---------------------------------------------------
          '
          Dim ParsingIsInAnErrorState = False ' This flag enables the parser to continue parsing whilst there is an issue found. Allowing us to report additional issue.
          ' -- Parse and Calculate IndexPart Value
          Dim ArgIndex = 0
          Do
            ' Was cancellation requestes? If so exit.
            If cancellationToken.IsCancellationRequested Then Exit Function
            If Not ParsingIsInAnErrorState Then
              ' Work out the new value of the Index
              ArgIndex = (10 * ArgIndex) + DigitValue(CurrentCharacter)
            End If
            CurrentPosition += 1
            If (CurrentPosition >= LengthOfTheText) Then Yield New UnexpectedlyReachedEndOfText : Exit Function
            CurrentCharacter = format(CurrentPosition)
            If Not ParsingIsInAnErrorState AndAlso (ArgIndex >= _LIMIT_) Then ParsingIsInAnErrorState = True
          Loop While IsDigit(CurrentCharacter)
          ' Why did we exit?
          ArgsCounted += 1
          EndPositionForThisPart = CurrentPosition - 1
          If ArgIndex >= _LIMIT_ Then
            ' Index Value is greater or equal to limit.
            Yield New ArgIndexHasExceedLimit("Arg Index", ArgIndex.ToString, _LIMIT_, StartPositionForThisPart, EndPositionForThisPart)
            If ExitOnFirst Then Exit Function
          End If
          If Not ParsingIsInAnErrorState AndAlso (ArgIndex >= NumOfArgs) Then
            ' Index is out of the bounds of the supplied args.
            Yield New ArgIndexOutOfRange(ArgIndex, NumOfArgs, StartPositionForThisPart, EndPositionForThisPart)
            ' ToDo: Get the Start and End positions of opening and closing braces.
            If ExitOnFirst Then Exit Function
          End If
          ' Reset the ParsingIsInAnErrorState Flag 
          ParsingIsInAnErrorState = False
          ConsumeSpaces(format, CurrentPosition, LengthOfTheText, CurrentCharacter, cancellationToken)
          If (CurrentPosition >= LengthOfTheText) Then Yield New UnexpectedlyReachedEndOfText : Exit Function
          '
          ' +-----------------------------------------
          ' | Start of Parsing for the AlignmentPart  
          ' +-----------------------------------------
          '
          Dim LeftJustifiy = False
          If CurrentCharacter = _COMMA_ Then
            CurrentPosition += 1
            ConsumeSpaces(format, CurrentPosition, LengthOfTheText, CurrentCharacter, cancellationToken)
            If (CurrentPosition >= LengthOfTheText) Then Yield New UnexpectedlyReachedEndOfText : Exit Function
            CurrentCharacter = format(CurrentPosition)
            If CurrentCharacter = _MINUS_ Then
              LeftJustifiy = True
              CurrentPosition += 1
              If (CurrentPosition >= LengthOfTheText) Then Yield New UnexpectedlyReachedEndOfText : Exit Function
              CurrentCharacter = format(CurrentPosition)
            End If
            ' If next Character after a minus, or currenct character isnot a Digit then it is an error
            If Not IsDigit(CurrentCharacter) Then
              Yield New UnexpectedChar(CurrentCharacter, CurrentPosition)
              If ExitOnFirst Then Exit Function
            End If
            ' Reset the markers for highlighter
            StartPositionForThisPart = CurrentPosition
            EndPositionForThisPart = CurrentPosition
            Dim Width = 0
            Do
              If cancellationToken.IsCancellationRequested Then Exit Function
              If Not ParsingIsInAnErrorState Then
                ' Calculate the new value of width
                Width = (10 * Width) + DigitValue(CurrentCharacter)
              End If
              CurrentPosition += 1
              If (CurrentPosition >= LengthOfTheText) Then
                Yield New UnexpectedlyReachedEndOfText
                Exit Function
              End If
              If Not ParsingIsInAnErrorState AndAlso (Width >= _LIMIT_) Then ParsingIsInAnErrorState = True
              CurrentCharacter = format(CurrentPosition)
            Loop While IsDigit(CurrentCharacter) 'AndAlso (Width < _LIMIT_)
            ' Why did we exit?
            EndPositionForThisPart = CurrentPosition - 1
            If Width >= _LIMIT_ Then
              ' Index Value is greater or equal to limit.
              Dim WidthText = format.Substring(StartPositionForThisPart, (EndPositionForThisPart - StartPositionForThisPart) + 1)
              Yield New ArgIndexHasExceedLimit("Value when limit was exceeded. ", WidthText, _LIMIT_, StartPositionForThisPart, EndPositionForThisPart)
              If ExitOnFirst Then Exit Function
            End If
          End If
          ConsumeSpaces(format, CurrentPosition, LengthOfTheText, CurrentCharacter, cancellationToken)
          If Not ParsingIsInAnErrorState AndAlso (CurrentPosition >= LengthOfTheText) Then Yield New UnexpectedlyReachedEndOfText : Exit Function
          '
          ' +--------------------------------------------
          ' |  Start of Parsing for formatting strings 
          ' +--------------------------------------------
          '
          If CurrentCharacter = _COLON_ Then
            CurrentPosition += 1
            While True
              If cancellationToken.IsCancellationRequested Then Exit Function
              If (CurrentPosition >= LengthOfTheText) Then Yield New UnexpectedlyReachedEndOfText : Exit Function
              CurrentCharacter = format(CurrentPosition)
              Select Case CurrentCharacter
                Case Opening_Brace
                  If (CurrentPosition < LengthOfTheText) AndAlso format(CurrentPosition) = Opening_Brace Then
                    ' This brace has escaped! {{
                    CurrentPosition += 1
                  Else
                    If (CurrentPosition >= LengthOfTheText) Then Yield New UnexpectedlyReachedEndOfText : Exit Function
                    Yield New UnexpectedChar(CurrentCharacter, CurrentPosition)
                    If ExitOnFirst Then Exit Function
                  End If
                Case Closing_Brace
                  If (CurrentPosition < LengthOfTheText) AndAlso format(CurrentPosition) = Closing_Brace Then
                    ' This brace has escaped! }}
                    CurrentPosition += 1
                  Else
                    If (CurrentPosition >= LengthOfTheText) Then Yield New UnexpectedlyReachedEndOfText : Exit Function

                    CurrentPosition -= 1
                    Exit While
                  End If
              End Select
            End While
          End If
          If CurrentCharacter <> Closing_Brace Then
            Yield New UnexpectedChar(CurrentCharacter, CurrentPosition)
            If ExitOnFirst Then Exit Function
          End If
          CurrentPosition += 1
        End While
        If ArgsCounted = 0 Then Yield New ContainsNoArgs
      Catch ex As Exception
        ' Let's use the IDE error window to also report internal errors, :-)
        internalError = New Internal_IssueReport(ex.ToString)
      End Try
      If internalError IsNot Nothing Then Yield internalError
    End Function
    Private Sub ConsumeSpaces(format As String, ByRef pos As Integer, len As Integer, ByRef Curr As Char, cancellationToken As CancellationToken)
      ' Consume spaces
      While (pos < len)
        If cancellationToken.IsCancellationRequested Then Exit Sub
        Curr = format(pos)
        If Curr <> _SPACE_ Then Exit While
        pos += 1
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
      MyBase.New("Unexpected Character (" & C & ")", Start)
    End Sub
  End Class
  Public Class ContainsNoArgs
    Inherits IssueReport
    Public Sub New()
      MyBase.New("")
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
End Namespace