Option Strict On

Imports AdamSpeight2008.StringFormatDiagnostics.Results
Imports AdamSpeight2008.StringFormatDiagnostics.Errors
Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Common
  Public Module SFD
    Public Iterator Function Yield_ArgHoles(theString As String, indexOffset As Integer) As IEnumerable(Of Results.Base_Result)
      If theString Is Nothing Then Return
      Dim sr As New StringReader(theString)
      Dim InHole As Boolean = False
      Dim InErrorState = False
      Dim IsQuoted = False
      While sr.IsNotEoT
        Select Case True
          Case (sr = "{"c) AndAlso (sr.Peek = "{"c) : IsQuoted = True
          Case (sr = "{"c) AndAlso Not InHole
            InHole = True
            Dim arg_Hole = Parse_ArgHole(sr)
            If (TypeOf arg_Hole Is Error_Result) OrElse (arg_Hole Is Nothing) Then InErrorState = True
            Yield arg_Hole
            If Not InErrorState Then InHole = False
          Case (sr = "{"c) AndAlso InHole ' Parsing Error: Recursize hole not allowed
            Yield New Error_Result(New UnexpectedChar(sr.Index, sr.Value.Value))
          Case (sr = "}"c) AndAlso (sr.Peek = "}"c) : IsQuoted = True
          Case (sr = "}"c)
            If Not InHole Then
              ' Parsing Error: Mismatched brace.
              Yield New Error_Result(New UnexpectedChar(sr.Index, sr.Value.Value))
            Else
              InHole = False
              InErrorState = False
            End If
        End Select
        If IsQuoted Then sr.Next() : IsQuoted = False
        sr.Next()
      End While
    End Function

    Private Function Parse_Arg_Index(sr As StringReader) As Base_Result
      If sr.IsDigit = False Then Return New Error_Result(New UnexpectedChar(sr.Index, sr.Value.Value))
      sr.Mark()
      Dim RawText = sr.Parse_Digits
      Dim sp = sr.Unmark
      Dim value = 0
      If Integer.TryParse(RawText, value) Then Return New Result(Of Arg_Identifier)(New Arg_Index(sp, value))
      Return New Error_Result(New NonInteger(sp.Value))
    End Function

    Private Function Parse_Arg_Alignment(sr As StringReader) As Base_Result
      Dim Raw = ""
      sr.Mark()
      If sr = "-"c Then Raw = "-"c : sr.Next()
      If sr.IsDigit Then
        sr.Mark()
        Raw &= sr.Parse_Digits
        Dim sp = sr.Unmark
        Dim value = 0
        If Integer.TryParse(Raw, value) Then Return New Result(Of Arg_Alignment)(New Arg_Alignment(sp, value))
        Return New Error_Result(New NonInteger(sp.Value))
      Else
        sr.Unmark()
        Return New Error_Result(New UnexpectedChar(sr.Index, sr.Value.Value))
      End If
    End Function

    Private Function Parse_Arg_Format(sr As StringReader) As Base_Result
      Dim sb As New System.Text.StringBuilder()
      sr.Mark()
      While sr.IsNotEoT AndAlso sr.Value <> "}"c
        sb.Append(sr.Value)
        sr.Next()
      End While
      Dim sp = sr.Unmark
      If sr.IsNotEoT Then sr.Next()
      Return New Result(Of Arg_Format)(New Arg_Format(sp, sb.ToString))
    End Function

    Private Function Parse_VariableName(sr As StringReader) As Base_Result
      '     Dim td = Microsoft.CodeAnalysis.FindSymbol
    End Function

    Private Function Parse_ArgHole(sr As StringReader) As Base_Result
      Dim LeftEdgeOfHole = sr.Index
      sr.Next()
      If sr.IsEoT Then Return New Error_Result(Errors.UnexpectedEoT.Default)

      Dim Index_Value As Arg_Identifier = Nothing
      Dim Align_Value As Arg_Alignment = Nothing
      Dim Format_Value As Arg_Format = Nothing
      If sr.IsDigit Then
        Dim res = Parse_Arg_Index(sr)
        If TypeOf res Is Error_Result Then Return res

        Index_Value = DirectCast(res, Result(Of Arg_Identifier)).Value
      Else
        ' Add parser for identifers here

      End If
      sr.Consume_Spaces
      ' Alignment 
      If sr = ","c Then
        sr.Next()
        If sr.IsEoT Then Return New Error_Result(Errors.UnexpectedEoT.Default)
        Dim res1 = Parse_Arg_Alignment(sr)
        If TypeOf res1 Is Error_Result Then
        Else
          Align_Value = DirectCast(res1, Result(Of Arg_Alignment)).Value
        End If
      End If
      ' Formatting
      If sr = ":"c Then
        sr.Next()
        Format_Value = DirectCast(Parse_Arg_Format(sr), Result(Of Arg_Format)).Value
      ElseIf sr = "}"c Then
      Else
        Return New Error_Result(New UnexpectedChar(sr.Index, sr.Value.Value))
      End If
      Return New Result(Of ArgHole)(New ArgHole(New IndexSpan(LeftEdgeOfHole, sr.Index), Index_Value, Align_Value, Format_Value))
    End Function

    <Runtime.CompilerServices.Extension>
    Public Function IsDigit(sr As StringReader) As Boolean
      Select Case sr.Value
        Case "0"c To "9"c : Return True
      End Select
      Return False
    End Function

    <Runtime.CompilerServices.Extension>
    Public Function Parse_Digits(sr As StringReader) As String
      If sr Is Nothing Then Return Nothing
      Dim sb As New System.Text.StringBuilder
      While sr.IsNotEoT AndAlso sr.IsDigit
        sb.Append(sr.Value)
        sr.Next()
      End While
      Return sb.ToString
    End Function

    <Runtime.CompilerServices.Extension>
    Function Consume_Spaces(sr As StringReader) As StringReader
      If sr Is Nothing Then Return sr
      While sr.IsNotEoT AndAlso (sr.Value = " "c)
        sr.Next()
      End While
      Return sr
    End Function

  End Module

End Namespace
