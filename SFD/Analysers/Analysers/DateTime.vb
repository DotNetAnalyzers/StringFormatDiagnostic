Option Strict On
Imports System.Threading
Imports AdamSpeight2008.StringFormatDiagnostics
Imports AdamSpeight2008.StringFormatDiagnostics.Results

Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Analysers

    <HideModuleName>
    Public Module DateTime
        Public Function Analyse_DateTime_ToString(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As Base_Result
      Dim _res_ As New Result(Of String)("")
      If format Is Nothing Then Return _res_ '.AddError(New _Internal.Warning(New ArgumentNullException("format").ToString)) : Return _res_
      Dim cf As ICustomFormatter = Nothing
      If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      If format.Length = 0 Then Return _res_

      If format.Length = 1 Then
        ' Standard Date and Time Format Strings (http://msdn.microsoft.com/en-us/library/az4se3k1(v=vs.110)
        If "dDfFgGmMoOrRstTuUyY".Contains(format(0)) Then
          ' Valid specifier
        Else
          _res_.AddError(New Errors.UnknownSpecifier(IndexOffset + 0, format(0)))
        End If
      Else
        ' Custom format string
        _res_.IncludeFrom(Analyse_Custom_DateTime(ct, format, IndexOffset, Provider, Args))
      End If
      '    _res_.LastParse = ??
      Return _res_
    End Function

        Private Function Analyse_Custom_DateTime(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As Base_Result
      Dim _res_ As New Result(Of String)("")
      '_res_.AddError(New Internal_Information("(DateTime) CustomFormatString Diagnostic Not yet Implemented."))
      If format Is Nothing Then Return _res_ '.AddError(New _Internal.Warning(New ArgumentNullException("format").ToString))
      Dim _ExitOnFirst_ = False
      Dim Curr As New StringReader(format)


      While Curr.IsNotEoT
        Select Case Curr.Value
          Case "d"c
            Dim reps = Curr.RepCount("d"c)
            _res_.IncludeFrom(reps)
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1 To 7
              Case Else
            End Select
            Curr = reps.SR
          Case "f"c
            Dim reps = Curr.RepCount("f"c)
            _res_.IncludeFrom(reps)
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1 To 7
              Case Else
            End Select
            Curr = reps.SR
          Case "F"c
            Dim reps = Curr.RepCount("F"c)
            _res_.IncludeFrom(reps)
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1 To 7
              Case Else
            End Select
            Curr = reps.SR
          Case "g"c
            Dim reps = Curr.RepCount("g"c)
            _res_.IncludeFrom(reps)
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New Errors.SpecifierUnknown(New String("g"c, reps.Value), IndexOffset + Curr.Index)) ', reps.LastParse.Index))
            End Select
            Curr = reps.SR
          Case "h"c
            Dim reps = Curr.RepCount("h"c)
            _res_.IncludeFrom(reps)
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
            End Select
            Curr = reps.SR
          Case "H"c
            Dim reps = Curr.RepCount("H"c)
            _res_.IncludeFrom(reps)
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New Errors.SpecifierUnknown(New String("H"c, reps.Value), IndexOffset + Curr.Index))
            End Select
            Curr = reps.SR
          Case "K"c
            Dim reps = Curr.RepCount("K"c)
            _res_.IncludeFrom(reps)
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New Errors.SpecifierUnknown(New String("K"c, reps.Value), IndexOffset + Curr.Index))
            End Select
            Curr = reps.SR

          Case "m"c
            Dim reps = Curr.RepCount("m"c)
            _res_.IncludeFrom(reps)

            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New Errors.SpecifierUnknown(New String("m"c, reps.Value), IndexOffset + Curr.Index))
            End Select
            Curr = reps.SR

          Case "M"c
            Dim reps = Curr.RepCount("M"c)
            _res_.IncludeFrom(reps)
            '        If reps.IsValid = False Then
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New Errors.SpecifierUnknown(New String("M"c, reps.Value), IndexOffset + Curr.Index))
            End Select
            Curr = reps.SR
          '       End If
          Case "s"c
            Dim reps = Curr.RepCount("s"c)
            _res_.IncludeFrom(reps)
            '            If reps.IsValid = False Then
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New Errors.SpecifierUnknown(New String("s"c, reps.Value), IndexOffset + Curr.Index))
            End Select
            Curr = reps.SR
          '            End If
          Case "t"c
            Dim reps = Curr.RepCount("t"c)
            _res_.IncludeFrom(reps)
            '           If reps.IsValid = False Then
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New Errors.SpecifierUnknown(New String("t"c, reps.Value), IndexOffset + Curr.Index))
            End Select
            Curr = reps.SR
          '          End If
          Case "y"c
            Dim reps = Curr.RepCount("y"c)
            _res_.IncludeFrom(reps)
            '       If reps.IsValid = False Then
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1 To 5
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New Errors.SpecifierUnknown(New String("y"c, reps.Value), IndexOffset + Curr.Index))
            End Select
            Curr = reps.SR
          '           End If
          Case "z"c
            Dim reps = Curr.RepCount("z"c)
            _res_.IncludeFrom(reps)
            '        If reps.IsValid = False Then
            Select Case reps.Value
              Case 0 ' Should never occure
              Case 1, 2, 3
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New Errors.SpecifierUnknown(New String("z"c, reps.Value), IndexOffset + Curr.Index))
            End Select
            Curr = reps.SR
          '       End If
          Case ":"c
            Dim reps = Curr.RepCount(":"c)
            _res_.IncludeFrom(reps)
            'If reps.IsValid = False Then
            If reps.Value <> 1 Then _res_.AddError(New Errors.SpecifierUnknown(New String(":"c, reps.Value), IndexOffset + Curr.Index))
            Curr = reps.SR
          '            End If
          Case "/"c
            Dim reps = Curr.RepCount("/"c)
            _res_.IncludeFrom(reps)
            '      If reps.IsValid = False Then
            If reps.Value <> 1 Then _res_.AddError(New Errors.SpecifierUnknown(New String("/"c, reps.Value), IndexOffset + Curr.Index))
            Curr = reps.SR
          '      End If
          Case "\"c

            If Curr.Peek.HasValue = False Then _res_.AddError(Errors.UnexpectedEoT.Default) : Return _res_
            Curr.Next()
            Curr.Next()
          Case "'"c, _QUOTE_
            Dim r = LiteralString(Curr, Curr.Value.Value)
            _res_.IncludeFrom(r)
            If r.Value = False Then Exit While
            Curr = r.SR
          Case "%"c
            Dim nc = Curr.Peek
            If nc.HasValue = False Then _res_.AddError(Errors.UnexpectedEoT.Default) : Return _res_
            If "dfFghHKmMstyz:/".Contains(nc.Value) Then
              Curr.Next()
            Else
              _res_.AddError(New Errors.UnexpectedChar(IndexOffset + Curr.Index, nc.Value))
            End If
          Case Else
            Curr.Next()
        End Select
      End While
      Return _res_ '.LastParse(Curr)
    End Function

    End Module
End Namespace