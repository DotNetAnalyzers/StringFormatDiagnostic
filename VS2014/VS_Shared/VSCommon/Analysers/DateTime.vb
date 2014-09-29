Option Strict On
Imports System.Threading
Imports AdamSpeight2008.StringFormatDiagnostic
Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces
Imports AdamSpeight2008.StringFormatDiagnostic.IssueReports
Imports AdamSpeight2008.StringFormatDiagnostic.Common

Namespace Global.AdamSpeight2008.StringFormatDiagnostic.Analysers

  <HideModuleName>
  Public Module DateTime
    Public Function Analyse_DateTime_ToString(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As OutputResult(Of String)
      Dim _res_ As New OutputResult(Of String)
      If format Is Nothing Then _res_.AddError(New _Internal.Warning(New ArgumentNullException("format").ToString)) : Return _res_
      Dim cf As ICustomFormatter = Nothing
      If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      If format.Length = 0 Then Return _res_

      If format.Length = 1 Then
        ' Standard Date and Time Format Strings (http://msdn.microsoft.com/en-us/library/az4se3k1(v=vs.110)
        If "dDfFgGmMoOrRstTuUyY".Contains(format(0)) Then
          ' Valid specifier
        Else
          _res_.AddError(New Errors.UnknownSpecifier(format(0), IndexOffset + 0))
        End If
      Else
        ' Custom format string
        _res_.IncludeErrorsFrom(Analyse_Custom_DateTime(ct, format, IndexOffset, Provider, Args))
      End If
      ''    _res_.LastParse = ??
      Return _res_
    End Function

    Private Function Analyse_Custom_DateTime(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As OutputResult(Of String)
      Dim _res_ As New OutputResult(Of String)
      '_res_.AddError(New Internal_Information("(DateTime) CustomFormatString Diagnostic Not yet Implemented."))
      If format Is Nothing Then Return _res_.AddError(New _Internal.Warning(New ArgumentNullException("format").ToString))
      Dim _ExitOnFirst_ = False
      Dim s As New TheSourceText(format)
      Dim Curr As IParsedChar = New ParsedChar(s, IndexOffset + 0)

      While Curr.IsNotEoT
        Select Case Curr.Value
          Case "d"c
            Dim reps = Curr.RepCount("d"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1 To 7
                Case Else
              End Select
              Curr = _res_.Last
            End If
          Case "f"c
            Dim reps = Curr.RepCount("f"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1 To 7
                Case Else
              End Select
              Curr = _res_.Last
            End If
          Case "F"c
            Dim reps = Curr.RepCount("F"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1 To 7
                Case Else
              End Select
              Curr = _res_.Last
            End If
          Case "g"c
            Dim reps = Curr.RepCount("g"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1, 2
                Case Else
                  ' Add an error unknown specifier
                  _res_.AddError(New Errors.SpecifierUnknown(New String("g"c, reps.Output), IndexOffset + Curr.Index)) ', reps.LastParse.Index))
              End Select
              Curr = _res_.Last
            End If
          Case "h"c
            Dim reps = Curr.RepCount("h"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1, 2
                Case Else
                  ' Add an error unknown specifier
              End Select
              Curr = _res_.Last
            End If
          Case "H"c
            Dim reps = Curr.RepCount("H"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1, 2
                Case Else
                  ' Add an error unknown specifier
                  _res_.AddError(New Errors.SpecifierUnknown(New String("H"c, reps.Output), IndexOffset + Curr.Index))
              End Select
              Curr = _res_.Last
            End If
          Case "K"c
            Dim reps = Curr.RepCount("K"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1, 2
                Case Else
                  ' Add an error unknown specifier
                  _res_.AddError(New Errors.SpecifierUnknown(New String("K"c, reps.Output), IndexOffset + Curr.Index))
              End Select
              Curr = _res_.Last
            End If
          Case "m"c
            Dim reps = Curr.RepCount("m"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1, 2
                Case Else
                  ' Add an error unknown specifier
                  _res_.AddError(New Errors.SpecifierUnknown(New String("m"c, reps.Output), IndexOffset + Curr.Index))
              End Select
              Curr = _res_.Last
            End If
          Case "M"c
            Dim reps = Curr.RepCount("M"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1, 2
                Case Else
                  ' Add an error unknown specifier
                  _res_.AddError(New Errors.SpecifierUnknown(New String("M"c, reps.Output), IndexOffset + Curr.Index))
              End Select
              Curr = _res_.Last
            End If
          Case "s"c
            Dim reps = Curr.RepCount("s"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1, 2
                Case Else
                  ' Add an error unknown specifier
                  _res_.AddError(New Errors.SpecifierUnknown(New String("s"c, reps.Output), IndexOffset + Curr.Index))
              End Select
              Curr = _res_.Last
            End If
          Case "t"c
            Dim reps = Curr.RepCount("t"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1, 2
                Case Else
                  ' Add an error unknown specifier
                  _res_.AddError(New Errors.SpecifierUnknown(New String("t"c, reps.Output), IndexOffset + Curr.Index))
              End Select
              Curr = _res_.Last
            End If
          Case "y"c
            Dim reps = Curr.RepCount("y"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1 To 5
                Case Else
                  ' Add an error unknown specifier
                  _res_.AddError(New Errors.SpecifierUnknown(New String("y"c, reps.Output), IndexOffset + Curr.Index))
              End Select
              Curr = _res_.Last
            End If
          Case "z"c
            Dim reps = Curr.RepCount("z"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              Select Case reps.Output
                Case 0 ' Should never occure
                Case 1, 2, 3
                Case Else
                  ' Add an error unknown specifier
                  _res_.AddError(New Errors.SpecifierUnknown(New String("z"c, reps.Output), IndexOffset + Curr.Index))
              End Select
              Curr = _res_.Last
            End If
          Case ":"c
            Dim reps = Curr.RepCount(":"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              If reps.Output <> 1 Then _res_.AddError(New Errors.SpecifierUnknown(New String(":"c, reps.Output), IndexOffset + Curr.Index))
              Curr = _res_.Last
            End If
          Case "/"c
            Dim reps = Curr.RepCount("/"c)
            _res_.IncludeErrorsFrom(reps)
            If reps.IsValid = False Then
              If reps.Output <> 1 Then _res_.AddError(New Errors.SpecifierUnknown(New String("/"c, reps.Output), IndexOffset + Curr.Index))
              Curr = _res_.Last
            End If
          Case "\"c
            Curr = Curr.Next
            If Curr.IsEoT Then Return _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default).LastParse(Curr)
            Curr = Curr.Next
          Case "'"c, _QUOTE_
            Dim r = LiteralString(Curr, Curr.Value)
            _res_.IncludeErrorsFrom(r)
            If r.IsValid = False Then Exit While
            Curr = r.Last
          Case "%"c
            Dim nc = Curr.Next
            If nc.IsEoT Then Return _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default).LastParse(Curr)
            If "dfFghHKmMstyz:/".Contains(nc.Value) Then
              Curr = nc
            Else
              _res_.AddError(New Errors.UnexpectedChar(nc.Value, IndexOffset + nc.Index))
            End If
          Case Else
            Curr = Curr.Next
        End Select
      End While
      Return _res_.LastParse(Curr)
    End Function

  End Module
End Namespace