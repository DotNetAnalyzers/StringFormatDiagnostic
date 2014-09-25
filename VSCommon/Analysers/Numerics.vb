Option Strict On
Imports System.Threading
Imports AdamSpeight2008.StringFormatDiagnostic
Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces
Imports AdamSpeight2008.StringFormatDiagnostic.IssueReports
Imports AdamSpeight2008.StringFormatDiagnostic.Common

Namespace Global.AdamSpeight2008.StringFormatDiagnostic.Analysers

  <HideModuleName>
  Public Module Numerics

    Private Function ExponentValue(pc As IParsedChar) As OutputResult(Of Integer)
      Dim _res_ As New OutputResult(Of Integer)
      If pc Is Nothing Then Return _res_.AddError(New _Internal.Warning(New ArgumentNullException("pc").ToString))
      Dim sp = pc
      Dim pr = ParseDigits(sp)
      If pr.Output.Length > 0 Then
        Dim value As Integer
        If Integer.TryParse(pr.Output, value) Then
          If value.IsBetween(0, 100) Then
            _res_.Output = value
          Else
            _res_.AddError(New Errors.ValueHasExceedLimit("Exponent", value, 99, sp.Index, pr.Last.Index))

          End If
        Else
        End If
      End If
      Return _res_.LastParse(pr.Last)
    End Function


    Private Function Analyse_Custom_Numeric(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As OutputResult(Of String)
      Dim _res_ As New OutputResult(Of String)
      If format Is Nothing Then Return _res_.AddError(New _Internal.Warning(New ArgumentNullException("format").ToString))
      Dim _ExitOnFirst_ = False
      Dim s As New TheSourceText(format)
      Dim Curr As IParsedChar = New ParsedChar(s, 0)
      Dim Decimal_Points = 0
      Dim Sections = 1

      While Curr.IsNotEoT
        If ct.IsCancellationRequested Then Exit While
        Select Case Curr.Value
          Case "0"c : Curr = Curr.Next '  Zero Placeholder
          Case "#"c : Curr = Curr.Next ' Digit Placeholder
          Case "."c ' Decimal Point
            If Decimal_Points > 0 Then
              _res_.AddError(New Warnings.IgnoredChar(Curr.Value, Curr.Index + IndexOffset))
              If _ExitOnFirst_ Then Exit While
            End If
            Decimal_Points += 1
            Curr = Curr.Next
          Case "%"c : Curr = Curr.Next ' Percentage Holder
          Case "‰"c : Curr = Curr.Next ' Per Mille Placeholder

          Case "E"c, "e"c ' Expotential Holder
            Curr = Curr.Next
            If Curr.IsEoT Then Return _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default).LastParse(Curr)
            Select Case Curr.Value
              Case "0"c To "9"c
                Dim pr = ExponentValue(Curr)
                _res_.IncludeErrorsFrom(pr)
                Curr = pr.Last
              Case "-"c
                Curr = Curr.Next
                If Curr.IsEoT Then Return _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default).LastParse(Curr)
                If Not Curr.IsDigit Then Return _res_.AddError(New Errors.UnexpectedChar(Curr.Value, IndexOffset + Curr.Index)).LastParse(Curr)
                Dim pr = ExponentValue(Curr)
                _res_.IncludeErrorsFrom(pr)
                Curr = pr.Last
              Case "+"c
                Curr = Curr.Next
                If Curr.IsEoT Then Return _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default).LastParse(Curr)
                If Not Curr.IsDigit Then Return _res_.AddError(New Errors.UnexpectedChar(Curr.Value, IndexOffset + Curr.Index)).LastParse(Curr)
                Dim pr = ExponentValue(Curr)
                _res_.IncludeErrorsFrom(pr)
                Curr = pr.Last
              Case Else
                _res_.AddError(New Errors.UnexpectedChar(Curr.Value, Curr.Index))
            End Select

          Case "'"c, _QUOTE_ ' Literal String Delimiter 
            ' The same character terminates parsing of the literal string eg 'abc'  || "abc"
            Curr = Curr.Next
            While Curr.IsEoT
              If (Curr.Value = "'"c) OrElse (Curr.Value = _QUOTE_) Then Curr = Curr.Next : Exit While
              Curr = Curr.Next
            End While

          Case ";"c ' Group Separator and Number Scaling
            If Sections >= 3 Then _res_.AddError(New Warnings.TooManySections(IndexOffset + Curr.Index))
            Sections += 1

            Curr = Curr.Next
          Case "\"c ' Escape Character
            Curr = Curr.Next
            If Curr.IsEoT Then Return _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default).LastParse(Curr)
            Select Case Curr.Value
              Case "\"c, "0"c, "#"c, "."c, "'"c, _QUOTE_, ";"c, "%"c, "‰"c
                Curr = Curr.Next
              Case Else
                ' To Chek: Could be a parsed error
                Curr = Curr.Next
            End Select
          Case Else ' All other characters
            Curr = Curr.Next
        End Select
      End While
      Return _res_.LastParse(Curr)
    End Function

    Public Function Analyse_Numeric_ToString(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As OutputResult(Of String)
      Dim _res_ As New OutputResult(Of String)
      If format Is Nothing Then _res_.AddError(New _Internal.Warning(New ArgumentNullException("format").ToString)) : Return _res_
      Dim cf As ICustomFormatter = Nothing
      If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      If format.Length > 0 Then
        If format.ContainsMoreThan(1, Function(c) Char.IsLetter(c) OrElse Char.IsWhiteSpace(c)) = False Then
          Const _SNFS_ = "CcDdEeFfGgNnPpRrXx"
          Select Case format.Length
            Case 0
            Case 1
              If _SNFS_.Contains(format(0)) Then
                ' ' Parsed as a standard format string.
              Else
                _res_.AddError(New Errors.UnknownSpecifier(format(0), IndexOffset + 0))
              End If
            Case 2
              If _SNFS_.Contains(format(0)) Then
                If format(1).IsDigit Then
                  ' Parsed as a standard format string.
                Else
                  ' Parse as a Custom Numeric format string
                  _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
                End If
              Else
                _res_.AddError(New Errors.UnknownSpecifier(format(0), IndexOffset + 0))
              End If
            Case 3
              If _SNFS_.Contains(format(0)) Then
                If format(1).IsDigit Then
                  If format(2).IsDigit Then
                    ' Parsed as a standard format string.
                  Else
                    ' Parse as a Custom Numeric format string
                    _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
                  End If
                Else
                  ' Parse as a Custom Numeric format string
                  _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
                End If
              Else
                _res_.AddError(New Errors.UnknownSpecifier(format(0), IndexOffset + 0))
              End If
            Case Else
              ' Parse as a Custom Numeric format string
              _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
          End Select

        Else
          ' parse custon numeric string.
          _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
        End If
      End If
      '   _res_.LastParse = ??
      Return _res_
    End Function



  End Module
End Namespace
