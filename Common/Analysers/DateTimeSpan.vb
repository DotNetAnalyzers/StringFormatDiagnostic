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
Imports AdamSpeight2008.StringFormatDiagnostic.Common
Imports Common

Namespace AdamSpeight2008.StringFormatDiagnostics.Analysers
  Public Module DateTimeSpan
    Private Function Analyse_Custom_TimeSpan(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As OutputResult(Of String)
      Dim _res_ As New OutputResult(Of String)
      '_res_.AddError(New Internal_Information("(TimeSpan) CustomFormatString Diagnostic Not yet Implemented."))
      If format Is Nothing Then _res_.AddError(New _Internal.Warning(New ArgumentNullException("format").ToString)) : Return _res_
      Dim Curr As IParsedChar = Nothing
      Const _TS_ = "dhmsfF"
      Select Case format.Length
        Case 0
        Case 1 : If _TS_.Contains(format(0)) = False Then _res_.AddError(New Errors.UnknownSpecifier(format(0), IndexOffset + 0))
        Case 2
          If Not ((format(0) = "%"c) AndAlso _TS_.Contains(format(1))) Then
            _res_.AddError(New Errors.UnknownSpecifier(format(1), 1))
          ElseIf Not ((format(0) = " "c) AndAlso _TS_.Contains(format(1))) Then
            _res_.AddError(New Errors.UnknownSpecifier(format(1), 1))
          ElseIf Not (_TS_.Contains(format(0)) AndAlso (format(1) = " "c)) Then
            _res_.AddError(New Errors.UnknownSpecifier(format(1), 1))
          End If
        Case Else
          Dim _ExitOnFirst_ = False
          Dim s As New TheSourceText(format)
          Curr = New ParsedChar(s, 0)
          While Curr.IsNotEoT
            Select Case Curr.Value
              Case "d"c
                Dim reps = Curr.RepCount("d"c)
                _res_.IncludeErrorsFrom(reps)
                If _res_.IsValid Then
                  Select Case reps.Output
                    Case 0 ' Should never occur
                    Case 1 To 8
                    Case Else
                      _res_.AddError(New Errors.SpecifierUnknown(New String("d"c, reps.Output), IndexOffset + Curr.Index)) ', reps.LastParse.Index))
                  End Select
                End If
                Curr = reps.Last
              Case "h"c
                Dim reps = Curr.RepCount("h"c)
                _res_.IncludeErrorsFrom(reps)
                If reps.IsValid Then
                  Select Case reps.Output
                    Case 0, 1, 2
                    Case Else
                      _res_.AddError(New Errors.SpecifierUnknown(New String("h"c, reps.Output), IndexOffset + Curr.Index)) ', reps.LastParse.Index))
                  End Select
                End If
                Curr = reps.Last
              Case "m"c
                Dim reps = Curr.RepCount("m"c)
                _res_.IncludeErrorsFrom(reps)
                If reps.IsValid Then
                  Select Case reps.Output
                    Case 0, 1, 2
                    Case Else
                      _res_.AddError(New Errors.SpecifierUnknown(New String("m"c, reps.Output), IndexOffset + Curr.Index)) ', reps.LastParse.Index))
                  End Select
                End If
                Curr = reps.Last
              Case "s"c
                Dim reps = Curr.RepCount("s"c)
                _res_.IncludeErrorsFrom(reps)
                If reps.IsValid Then
                  Select Case reps.Output
                    Case 0, 1, 2
                    Case Else
                      _res_.AddError(New Errors.SpecifierUnknown(New String("s"c, reps.Output), IndexOffset + Curr.Index)) ', reps.LastParse.Index))
                  End Select
                End If
                Curr = reps.Last
              Case "f"c
                Dim reps = Curr.RepCount("f"c)
                _res_.IncludeErrorsFrom(reps)
                If reps.IsValid Then
                  Select Case reps.Output
                    Case 0 To 7
                    Case Else
                      _res_.AddError(New Errors.SpecifierUnknown(New String("f"c, reps.Output), IndexOffset + Curr.Index)) ', reps.LastParse.Index))
                  End Select
                End If
                Curr = reps.Last
              Case "F"c
                Dim reps = Curr.RepCount("F"c)
                _res_.IncludeErrorsFrom(reps)
                If reps.IsValid Then
                  Select Case reps.Output
                    Case 0, 1, 2
                    Case Else
                      _res_.AddError(New Errors.SpecifierUnknown(New String("F"c, reps.Output), IndexOffset + Curr.Index)) ', reps.LastParse.Index))
                  End Select
                End If
                Curr = reps.Last
              Case "'"c
                Dim r = LiteralString(Curr, Curr.Value)
                _res_.IncludeErrorsFrom(r)
                If r.IsValid = False Then Exit While
                Curr = r.Last
              Case "\"c
                If Curr.Next.IsEoT Then _res_.AddError(Errors.UnexpectedlyReachedEndOfText.Default) : Exit While
                Curr = Curr.Next.Next
              Case Else
                ' NOTE: There is potential for this to be incorrect 
                _res_.AddError(New Errors.UnexpectedChar(Curr.Value, IndexOffset + Curr.Index))
                Exit While
            End Select

          End While
      End Select
      Return _res_.LastParse(Curr)
    End Function

    Public Function Analyse_TimeSpan_ToString(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As OutputResult(Of String)
      Dim _res_ As New OutputResult(Of String)
      If format Is Nothing Then _res_.AddError(New _Internal.Warning(New ArgumentNullException("fs").ToString)) : Return _res_

      Dim cf As ICustomFormatter = Nothing
      If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      If format.Length = 0 Then Return _res_
      If format.Length = 1 Then
        ' Standard TimeSpan Format Strings (http://msdn.microsoft.com/en-us/library/ee372286(v=vs.110)
        If "cgG".Contains(format(0)) Then
          ' Valid specifier
        Else
          _res_.AddError(New Errors.UnknownSpecifier(format(0), 0 + IndexOffset))
        End If
      Else
        ' Custom format string
        _res_.IncludeErrorsFrom(Analyse_Custom_TimeSpan(ct, format, IndexOffset, Provider, Args))
      End If
      '    _res_.LastParse = ??
      Return _res_
    End Function

  End Module
End Namespace
