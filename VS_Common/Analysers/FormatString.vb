Option Strict On
Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
'Imports Roslyn.StringFormatDiagnostics
Imports AdamSpeight2008.StringFormatDiagnostic
Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces
Imports AdamSpeight2008.StringFormatDiagnostic.IssueReports
Imports AdamSpeight2008.StringFormatDiagnostic.Common.Common
Imports Common
Namespace Global.AdamSpeight2008.StringFormatDiagnostic.Common
  Public Module Common_StringFormat

    Public Function AnalyseFormatString(ct As CancellationToken,
                                            format As String,
                                            IndexOffset As Integer,
                                            Provider As IFormatProvider,
                                            Args As IEnumerable(Of Object)) As OutputResult(Of String)
      'If format Is Nothing Then Throw New ArgumentNullException("fs")
      'If Args Is Nothing Then Throw New ArgumentNullException("Args")
      '
      ' This is based on the .net framework implementation of String.Format.
      '
      ' Rough Grammar Rules
      ' 
      '         Digit ::= '0' - '9'
      '        Spaces ::= ' '*
      '     IndexPart ::= Spaces Digit Spaces
      ' AlignmentPart ::= _Comma_ Spaces _MINUS_? Digit* Spaces
      '    FormatPart ::= _COLON_ Spaces ?? Spaces
      '  FormatString ::= Opening_Brace IndexPart AlignmentPart? FormatPart? Closing_Brace 
      '
      '
      Dim _out_ As New OutputResult(Of String)
      Dim _res_ As New OutputResult(Of System.Text.StringBuilder)
      _res_.Output = New System.Text.StringBuilder()
      If format Is Nothing Then
        _out_.Output = ""
        Return _out_.IncludeErrorsFrom(_res_)
      End If
      Dim NumOfArgs = If(Args Is Nothing, 0, Args.Count)


      Dim curr As IParsedChar = New ParsedChar(New TheSourceText(format), 0)
      Dim ArgsSupplied = NumOfArgs > 0
      Dim ArgsCounted = 0
      Dim internalError As IReportIssue = Nothing ' _Internal.Warning ' Internal_IssueReport = Nothing
      Dim _internalError As String = Nothing
      Dim Width As New OutputResult(Of Integer)
      Dim cf As ICustomFormatter = Nothing
      If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      Try
        While True
          Dim InvalidIndex As Boolean = False
          Dim ParsingIsInAnErrorState = False ' This flag enables the parser to continue parsing whilst there is an issue found. Allowing us to report additional issue.
          Dim StartPositionForThisPart = 0
          Dim EndPositionForThisPart = 0
          While curr IsNot Nothing
            Select Case curr.Value
              Case Closing_Brace
                If (curr IsNot Nothing) AndAlso (curr.Next IsNot Nothing) AndAlso (curr.Next.Value = Closing_Brace) Then
                  ' This brace has escaped! }}
                  curr = curr.Next
                Else
                  If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function
                  _res_.AddError(New Errors.UnexpectedChar(curr.Value, curr.Index))
                  If ExitOnFirst Then GoTo Exit_Function
                End If
              Case Opening_Brace
                If (curr IsNot Nothing) AndAlso (curr.Next IsNot Nothing) AndAlso (curr.Next.Value = Opening_Brace) Then
                  ' This brace has escaped! {{
                  curr = curr.Next
                Else
                  If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function
                  StartPositionForThisPart = curr.Index + 1  ' This is the char index of the first character in the IndexPart
                  Exit While
                End If
            End Select
            'Normally here we would Append( Curr ) but this is just checking the validity of the formatstring.      
            _res_.Output.Append(curr.Value)
            curr = curr.Next
            If ct.IsCancellationRequested Then GoTo Exit_Function
          End While
          ' Have we reached the end of the format string?
          If curr Is Nothing Then Exit While
          curr = curr.Next
          If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function
          ' Get current character of the text.
          'CurrentCharacter = fr.Curr.Value
          If ArgsSupplied AndAlso Not IsDigit(curr) Then
            ParsingIsInAnErrorState = True
            _res_.AddError(New Errors.UnexpectedChar(curr.Value, IndexOffset + curr.Index))
            If ExitOnFirst Then GoTo Exit_Function
          End If
          Dim StartIndexOFHole = curr.Index 
          ' #######################################################################
          ' +---------------------------------------------------
          ' | Start Parsing for the Index value
          ' +---------------------------------------------------
          '
          ' -- Parse and Calculate IndexPart Value
          '

          Dim ArgIndex = ParseValue(curr, ct, _LIMIT_, ParsingIsInAnErrorState)
          _res_.IncludeErrorsFrom(ArgIndex)
          curr = ArgIndex.Last
          ' Why did we exit?
          ArgsCounted += 1
          EndPositionForThisPart = curr.Index - 1
          If ArgsSupplied AndAlso ArgIndex.Output >= _LIMIT_ Then
            ' Index Value is greater or equal to limit.
            _res_.AddError(New Errors.ArgIndexHasExceedLimit("ArgIndex", ArgIndex.Output, _LIMIT_, IndexOffset + StartPositionForThisPart, IndexOffset + EndPositionForThisPart)) ' NOTE: Check API
            InvalidIndex = True
            If ExitOnFirst Then GoTo Exit_Function
          End If
          If ArgsSupplied AndAlso Not ParsingIsInAnErrorState AndAlso (ArgIndex.Output >= NumOfArgs) Then
            ' Index is out of the bounds of the supplied args.
            _res_.AddError(New Errors.ArgIndexOutOfRange("ArgIndex", ArgIndex.Output, NumOfArgs, IndexOffset + StartPositionForThisPart, IndexOffset + EndPositionForThisPart))
            ' ToDo: Get the Start and End positions of opening and closing braces.
            InvalidIndex = True
            If ExitOnFirst Then GoTo Exit_Function
          End If
          ' Reset the ParsingIsInAnErrorState Flag 
          ParsingIsInAnErrorState = False
          ConsumeSpaces(curr, ct)
          If curr Is Nothing Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function

          ' #######################################################################
          ' +-----------------------------------------
          ' | Start of Parsing for the AlignmentPart  
          ' +-----------------------------------------
          '
          Dim LeftJustifiy = False
          If curr.Value = _COMMA_ Then
            curr = curr.Next
            ConsumeSpaces(curr, ct)
            If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function
            'CurrentCharacter = fr.Curr.Value
            If curr.Value = _MINUS_ Then
              LeftJustifiy = True
              curr = curr.Next
              If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function

              'CurrentCharacter = fr.Curr.Value
            End If
            ' If next Character after a minus, or currenct character isnot a Digit then it is an error
            If ArgsSupplied AndAlso Not IsDigit(curr) Then
              ParsingIsInAnErrorState = True
              _res_.AddError(New Errors.UnexpectedChar(curr.Value, curr.Index))
              If ExitOnFirst Then GoTo Exit_Function
            End If
            ' Reset the markers for highlighter
            StartPositionForThisPart = curr.Index
            EndPositionForThisPart = curr.Index
            Width = ParseValue(curr, ct, _LIMIT_, ParsingIsInAnErrorState)
            _res_.IncludeErrorsFrom(Width)
            curr = Width.Last
            ' Why did we exit?
            EndPositionForThisPart = curr.Index - 1
            If ArgsSupplied AndAlso Width.Output >= _LIMIT_ Then
              ' Index Value is greater or equal to limit.
              Dim WidthText = format.Substring(StartPositionForThisPart, (EndPositionForThisPart - StartPositionForThisPart) + 1)
              _res_.AddError(New Errors.ArgIndexHasExceedLimit("Value when limit was exceeded. ", Width.Output, _LIMIT_, IndexOffset + StartPositionForThisPart, IndexOffset + EndPositionForThisPart))
              If ExitOnFirst Then GoTo Exit_Function
            End If
          End If
          ConsumeSpaces(curr, ct)
          If ArgsSupplied AndAlso Not ParsingIsInAnErrorState AndAlso (curr Is Nothing) Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function
          Dim _ResultOfParsingFormatString = Parse_Format(curr, ct, ArgsSupplied)
          _res_.IncludeErrorsFrom(_ResultOfParsingFormatString)
          curr = _ResultOfParsingFormatString.Last
          Dim IndexOfFormat = curr.Index 
          Dim fmt = _ResultOfParsingFormatString.Output


          If curr.Value <> Closing_Brace Then
            If ArgsSupplied Then
              ParsingIsInAnErrorState = True
              _res_.AddError(New Errors.UnexpectedChar(curr.Value, IndexOffset + curr.Index))
            End If
            If ExitOnFirst Then GoTo Exit_Function
          End If
          curr = curr.Next()
          '
          ' NOTE: Probably don't need to following for just checking the validation
          '
          Dim sFmt As String = Nothing
          Dim s As String = Nothing
          Dim arg = If(InvalidIndex = True, "", Args(ArgIndex.Output))
          If cf IsNot Nothing Then
            If fmt IsNot Nothing Then sFmt = fmt.ToString
            s = cf.Format(sFmt, arg, Provider)
          End If
          If s Is Nothing Then
            Dim formattableArg As IFormattable = TryCast(arg, IFormattable)
            If formattableArg IsNot Nothing Then
              If (sFmt Is Nothing) AndAlso (fmt IsNot Nothing) Then sFmt = fmt.ToString()
              ' IDEA: Add a format analyser here.

              '
              ' Check Validity of this parsed format string for the TypeOF args[ ArgIndex ] 
              '
              Dim result = Valid_Format_For_Type(ct,sFmt, arg,(IndexOfFormat-StartIndexOFHole)+1 )
              _res_.IncludeErrorsFrom(result)
              Dim rc = result.Errors.Where(Function(xx) xx.Level=DiagnosticSeverity.Error OrElse xx.Level=DiagnosticSeverity.Warning ).Count
              If rc > 0 Then
                s = ""
              Else
                s = formattableArg.ToString(sFmt, Provider)
              End If
              '
              '
              '
'              s = formattableArg.ToString(sFmt, Provider)
            ElseIf arg IsNot Nothing Then
              s = arg.ToString
            End If
          End If
          ' apply the alignment
          If s Is Nothing Then s = String.Empty
          Dim pad = Width.Output - s.Length
          If (Not LeftJustifiy) AndAlso (pad > 0) Then _res_.Output.Append(_SPACE_, pad)
          _res_.Output.Append(s)
          If LeftJustifiy AndAlso (pad > 0) Then _res_.Output.Append(_SPACE_, pad)
        End While
        If ArgsSupplied AndAlso ArgsCounted = 0 Then _res_.AddError(Info.ContainsNoArgs.Default)
        If Not (ArgsSupplied) AndAlso ArgsCounted > 0 Then _res_.AddError(Info.ContainsNoParameters.Default)

      Catch ex As Exception
        ' Let's use the IDE error window to also report internal errors, :-)
        internalError = New _Internal.Warning(ex.ToString)
      End Try
Exit_Function:
      If internalError IsNot Nothing Then _res_.AddError(internalError)
      Return _out_.IncludeErrorsFrom(_res_.AddError(New Info.FinalOutput(_res_.Output.ToString)).LastParse(curr)).LastParse(_res_.Last)
    End Function

    Private Function Valid_Format_For_Type(ct As CancellationToken,
                                          fs As String, obj As Object,i As Integer ) As OutputResult(Of String)
      Dim Result As New OutputResult(Of String)
      Result.Output = ""
      Dim objTN = obj.GetType.ToString
      Dim toStringMehods = From a In ToStringAnalysers  
                           Where a.Key =objTN
                           Select a.Value 
     If toStringMehods.Any Then
            Dim _Res_ = toStringMehods(0)(ct,fs,i,Nothing,{})
        Result.IncludeErrorsFrom(_Res_)
      End If


          Return Result
    End Function


    Private Function Parse_Format(curr As IParsedChar, ct As CancellationToken, ArgsSupplied As Boolean) As OutputResult(Of System.Text.StringBuilder)
      Dim _res_ As New OutputResult(Of System.Text.StringBuilder)
      '
      ' +--------------------------------------------
      ' |  Start of Parsing for formatting strings 
      ' +--------------------------------------------
      '
      ' IDEA: Extend Analysis to the check the type's format strings.
      '
      Dim fmt As System.Text.StringBuilder = Nothing
      If curr.Value = _COLON_ Then
        curr = curr.Next
        While True
          If ct.IsCancellationRequested Then GoTo Exit_Function
          If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function
          Select Case curr.Value
            Case Opening_Brace
              If (curr IsNot Nothing) AndAlso (curr.Next IsNot Nothing) AndAlso (curr.Next.Value = Opening_Brace) Then
                ' This brace has escaped! {{
                curr = curr.Next
              Else
                If ArgsSupplied Then
                  If curr Is Nothing Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function
                  _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default)
                End If

                If ExitOnFirst Then GoTo Exit_Function
              End If
            Case Closing_Brace
              If (curr IsNot Nothing) AndAlso (curr.Next IsNot Nothing) AndAlso (curr.Next.Value = Closing_Brace) Then
                ' This brace has escaped! }}
                curr = curr.Next
              Else
                If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : GoTo Exit_Function
                Exit While
              End If
          End Select
          If fmt Is Nothing Then fmt = New System.Text.StringBuilder
          fmt.Append(curr.Value)
          curr = curr.Next
        End While

      End If
Exit_Function:
      _res_.Output = fmt
      Return _res_.LastParse(curr)
    End Function

  End Module

End Namespace
