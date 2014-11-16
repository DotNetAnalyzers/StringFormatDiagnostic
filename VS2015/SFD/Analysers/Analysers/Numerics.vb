Option Strict On
Imports System.Threading
Imports AdamSpeight2008.StringFormatDiagnostics.Results

Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Analysers

  <HideModuleName>
  Public Module Numerics

    Public Function ParseDigits(pc As StringReader) As ParseResult(Of String)
      Dim sp = pc.Copy
      Dim res As New ParseResult(Of String)(sp, "")
      While sp.IsNotEoT AndAlso Char.IsDigit(sp.Value.Value)
        res.Value &= sp.Value.Value
        sp.Next()
      End While
      Return res
    End Function

    Private Function ExponentValue(pc As StringReader, offset As Integer) As ParseResult(Of Integer)
      Dim sp = pc.Copy
      Dim _res_ As New ParseResult(Of Integer)(sp, 0)
      'If pc Is Nothing Then Return _res_.AddError(New _Internal.Warning(New ArgumentNullException("pc").ToString))

      Dim pr = ParseDigits(sp)
      If pr.Value.Length > 0 Then
        Dim value As Integer
        If Integer.TryParse(pr.Value, value) Then
          If value.IsBetween(0, 100) Then
            _res_.Value = value
          Else
            _res_.AddError(New Errors.ValueHasExceededLimit("Exponent", New IndexSpan(pc.Index + offset, (pr.SR.Index - sp.Index) + 1), value, 100))
          End If
        Else
        End If
      Else
      End If
      Return _res_ '.LastParse(pr.Last)
    End Function


    Private Function Analyse_Custom_Numeric(ct As CancellationToken,
                                                  format As String,
                                             IndexOffset As Integer,
                                                Provider As IFormatProvider,
                                                    Args As IEnumerable(Of Object)
                                           ) As Base_Result
      Dim _res_ As New Result(Of String)("")
      If format Is Nothing Then Return _res_ '.AddError(New _Internal.Warning(New ArgumentNullException("format").ToString))
      Dim Curr As New StringReader(format)
      Dim Decimal_Points = 0
      Dim Sections = 1
      While Curr.IsNotEoT
        If ct.IsCancellationRequested Then Exit While
        Select Case Curr.Value
          Case "0"c : Curr.Next() '  Zero Placeholder
          Case "#"c : Curr.Next() ' Digit Placeholder
          Case "."c ' Decimal Point
            If Decimal_Points > 0 Then _res_.AddWarning(New Warnings.IgnoredChar(Curr.Index + IndexOffset, Curr.Value.Value))
            Decimal_Points += 1
            Curr.Next()
          Case "%"c : Curr.Next() ' Percentage Holder
          Case "‰"c : Curr.Next() ' Per Mille Placeholder
          Case "E"c, "e"c ' Expotential Holder
            Curr.Next()
            If Curr.IsEoT Then _res_.AddError(Errors.UnexpectedEoT.Default) : Return _res_
            Select Case Curr.Value
              Case "0"c To "9"c
                Dim pr = ExponentValue(Curr, IndexOffset)
                _res_.IncludeFrom(pr)
                Curr = pr.SR
              Case "-"c
                Curr.Next()
                If Curr.IsEoT Then _res_.AddError(Errors.UnexpectedEoT.Default) : Return _res_
                If Not Char.IsDigit(Curr.Value.Value) Then _res_.AddError(New Errors.UnexpectedChar(IndexOffset + Curr.Index, Curr.Value.Value)) : Return _res_
                Dim pr = ExponentValue(Curr, IndexOffset)
                _res_.IncludeFrom(pr)
                Curr = pr.SR
              Case "+"c
                Curr.Next()
                If Curr.IsEoT Then _res_.AddError(Errors.UnexpectedEoT.Default) : Return _res_
                If Char.IsDigit(Curr.Value.Value) Then _res_.AddError(New Errors.UnexpectedChar(IndexOffset + Curr.Index, Curr.Value.Value)) : Return _res_
                Dim pr = ExponentValue(Curr, IndexOffset)
                _res_.IncludeFrom(pr)
                Curr = pr.SR
              Case Else
                _res_.AddError(New Errors.UnexpectedChar(Curr.Index, Curr.Value.Value))
            End Select
          Case "'"c, _QUOTE_ ' Literal String Delimiter 
            ' The same character terminates parsing of the literal string eg 'abc'  || "abc"
            Curr.Next()
            While Curr.IsEoT
              If (Curr.Value = "'"c) OrElse (Curr.Value = _QUOTE_) Then Curr.Next() : Exit While
              Curr.Next()
            End While
          Case ";"c ' Group Separator and Number Scaling
            If Sections >= 3 Then _res_.AddWarning(New Warnings.TooManySections(IndexOffset + Curr.Index))
            Sections += 1
            Curr.Next()
          Case "\"c ' Escape Character
            Curr.Next()
            If Curr.IsEoT Then _res_.AddError(Errors.UnexpectedEoT.Default) : Return _res_
            Select Case Curr.Value
              Case "\"c, "0"c, "#"c, "."c, "'"c, _QUOTE_, ";"c, "%"c, "‰"c
                Curr.Next()
              Case Else
                ' To Chek: Could be a parsed error
                Curr.Next()
            End Select
          Case Else ' All other characters
            Curr.Next()
        End Select
      End While
      Return _res_ '.LastParse(Curr)
    End Function

    Public Function Analyse_Numeric_ToString(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As Base_Result
      Dim _res_ As New Result(Of String)("")
      If format Is Nothing Then Return _res_
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
                _res_.AddError(New Errors.UnknownSpecifier(IndexOffset + 0, format(0)))
              End If
            Case 2
              If _SNFS_.Contains(format(0)) Then
                If Char.IsDigit(format(1)) Then
                  ' Parsed as a standard format string.
                Else
                  ' Parse as a Custom Numeric format string
                  _res_.IncludeFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
                End If
              Else
                _res_.AddError(New Errors.UnknownSpecifier(IndexOffset + 0, format(0)))
              End If
            Case 3
              If _SNFS_.Contains(format(0)) Then
                If Char.IsDigit(format(1)) Then
                  If Char.IsDigit(format(2)) Then
                    ' Parsed as a standard format string.
                  Else
                    ' Parse as a Custom Numeric format string
                    _res_.IncludeFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
                  End If
                Else
                  ' Parse as a Custom Numeric format string
                  _res_.IncludeFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
                End If
              Else
                _res_.AddError(New Errors.UnknownSpecifier(IndexOffset + 0, format(0)))
              End If
            Case Else
              ' Parse as a Custom Numeric format string
              _res_.IncludeFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
          End Select

        Else
          ' parse custon numeric string.
          _res_.IncludeFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
        End If
      End If
      Return _res_
    End Function

  End Module

End Namespace
