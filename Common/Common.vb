Option Strict On
Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
Imports Roslyn.StringFormatDiagnostics

Public Module Common


  Public Const DiagnosticId = "String.Format Diagnostic"
  Public Const Description = "Is the formatstring valid?"
  Public Const MessageFormat = "Invalid FormatString (Reason: {0})"
  Public Const Category = "Validation"
  Public Rule1 As New DiagnosticDescriptor(id:=DiagnosticId,
                                           description:=Description,
                                           messageFormat:=MessageFormat,
                                           category:=Category,
                                           defaultSeverity:=DiagnosticSeverity.Error, isEnabledByDefault:=True)
  Public Rule2 As New DiagnosticDescriptor(id:=DiagnosticId,
                                           description:=Description,
                                           messageFormat:="This Constant is used as a FormatString" + Environment.NewLine + MessageFormat,
                                           category:=Category,
                                           defaultSeverity:=DiagnosticSeverity.Error,
                                           isEnabledByDefault:=True)

  Public ReadOnly Property TheSimpleOnes() As DiagMeth()
    Get
      Return _TheSimpleOnes
    End Get
  End Property

  Private _TheSimpleOnes As DiagMeth() = {New DiagMeth("System.Console", {"Write", "WriteLine"}),
                New DiagMeth("System.Diagnostics.Debug", {"WriteLine"}),
                New DiagMeth("System.IO.TextWriter", {"WriteLine"}),
                New DiagMeth("System.Diagnostics.Trace", {"TraceError", "TraceInformation", "TraceWarning"}),
                New DiagMeth("System.Diagnostics.TraceSource", {"TraceInformation"})}
'                New DiagMeth("System.String", {"Format"})}

  Public Function AddWarning(node As SyntaxNode, offset As Integer, endoffset As Integer, ri As IssueReport) As Diagnostic
    Return Diagnostic.Create(Rule1,
                             Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), ri.Message)
  End Function


  Public Function AddWarningAtSource(node As SyntaxNode, offset As Integer, endoffset As Integer, ri As IssueReport) As Diagnostic
    Return Diagnostic.Create(Rule2,
                             Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), ri.Message)
  End Function


  Public Function AddInformation(node As SyntaxNode, msg As String) As Diagnostic
    Return Diagnostic.Create(id:=DiagnosticId,
                             category:=Category,
                             message:=msg,
                             severity:=DiagnosticSeverity.Info,
                             isEnabledByDefault:=True,
                             warningLevel:=0,
                             isWarningAsError:=False,
                             location:=Location.Create(node.SyntaxTree, node.Span))
  End Function

#Region "Coomonly used Characters"
  Const Opening_Brace As Char = "{"c
  Const Closing_Brace As Char = "}"c
  Const _SPACE_ As Char = " "c
  Const _COMMA_ As Char = ","c
  Const _COLON_ As Char = ":"c
  Const _MINUS_ As Char = "-"c
  Const _QUOTE_ As Char = """"c
#End Region
  Const _LIMIT_ As Integer = 1000000  ' This limit is found inside the .net implementation of String.Format.
  Const ExitOnFirst = False

  Public Function LiteralString(pc As ParsedChar, q As Char) As OutputResult(Of Boolean)
    Dim res As New OutputResult(Of Boolean)
    Dim curr = pc.Next
    While curr.IsEoT AndAlso res.Output = False
      If curr = q Then res.Output = True : Exit While
      curr = curr.Next
    End While
    If Not res.Output Then res.AddError(New UnexpectedlyReachedEndOfText)
    res.LastParse = curr
    Return res
  End Function


  Public Function DeString(s As String) As String
    ' If a string is included in double qoutes (") remove the match pair.
    If s Is Nothing Then Return ""
    If (s.Length > 0) AndAlso (s.Last = _QUOTE_) Then s = s.Substring(0, s.Length - 1)
    If (s.Length > 0) AndAlso (s.First = _QUOTE_) Then s = s.Substring(1)
    Return s
  End Function

  Private Function ExponentValue(pc As ParsedChar) As OutputResult(Of Integer)
    Dim _res_ As New OutputResult(Of Integer)
    Dim sp = pc
    Dim pr = ParseDigits(sp)
    If pr.Output.Length > 0 Then
      Dim value As Integer '.TrimStart("0"c)
      If Integer.TryParse(pr.Output, value) Then
        If value < 0 Then
        ElseIf value > 99 Then
          _res_.AddError(New ValueHasExceedLimit("Exponent", pr.Output, 99, sp.Index, pr.LastParse.Index))
        Else
          _res_.Output = value
        End If
      Else
      End If
    End If
    _res_.LastParse = pr.LastParse
    Return _res_
  End Function

  Private Function Analyse_Custom_Numeric(ct As CancellationToken, format As String, Optional Provider As IFormatProvider = Nothing) As OutputResult(Of String)
    Dim _res_ As New OutputResult(Of String)
    '_res_.AddError(New Internal_Information("(Numeric) CustomFormatString Diagnostic Not yet Implemented."))
    Dim _ExitOnFirst_ = False
    Dim s As New TheSourceText(format)
    Dim Curr As New ParsedChar(s, 0)
    Dim Decimal_Points = 0
    Dim Sections = 1

    While Curr.IsNotEoT
      If ct.IsCancellationRequested Then Exit While
      Select Case Curr.Value
        Case "0"c ' Zero Placeholder
          Curr = Curr.Next
        Case "#"c ' Digit Placeholder
          Curr = Curr.Next
        Case "."c ' Decimal Point
          If Decimal_Points > 0 Then
            _res_.AddError(New IgnoredChar(Curr.Value, Curr.Index))
            If _ExitOnFirst_ Then Exit While
          End If
          Decimal_Points += 1
          Curr = Curr.Next
        Case "%"c ' Percentage Holder
          Curr = Curr.Next
        Case "‰"c ' Per Mille Placeholder
          Curr = Curr.Next
        Case "E"c, "e"c ' Expotential Holder
          Curr = Curr.Next
          If Curr.IsEoT Then _res_.AddError(New UnexpectedlyReachedEndOfText()) : Exit Select
          Select Case Curr
            Case "0"c To "9"c
              Dim pr = ExponentValue(Curr)
              _res_.IncludeErrorsFrom(pr)
              Curr = pr.LastParse
            Case "-"c
              Curr = Curr.Next
              If Curr.IsEoT Then _res_.AddError(New UnexpectedlyReachedEndOfText()) : Exit Select
              If Not Curr.IsDigit Then _res_.AddError(New UnexpectedChar(Curr.Value, Curr.Index)) : Exit Select
              Dim pr = ExponentValue(Curr)
              _res_.IncludeErrorsFrom(pr)
              Curr = pr.LastParse
            Case "+"c
              Curr = Curr.Next
              If Curr.IsEoT Then _res_.AddError(New UnexpectedlyReachedEndOfText()) : Exit Select
              If Not Curr.IsDigit Then _res_.AddError(New UnexpectedChar(Curr.Value, Curr.Index)) : Exit Select
              Dim pr = ExponentValue(Curr)
              _res_.IncludeErrorsFrom(pr)
              Curr = pr.LastParse
            Case Else
              _res_.AddError(New UnexpectedChar(Curr.Value, Curr.Index))
          End Select

        Case "'"c, _QUOTE_ ' Literal String Delimiter 
          ' The same character terminates parsing of the literal string eg 'abc'  || "abc"
          Curr = Curr.Next
          While Curr.IsEoT
            If (Curr = "'"c) OrElse (Curr = _QUOTE_) Then Curr = Curr.Next : Exit While
            Curr = Curr.Next
          End While

        Case ";"c ' Group Separator and Number Scaling
          If Sections >= 3 Then _res_.AddError(New TooManySections(Curr.Index)) ': Exit While
          Sections += 1

          Curr = Curr.Next
        Case "\"c ' Escape Character
          Curr = Curr.Next
          If Curr.IsEoT Then _res_.AddError(New UnexpectedlyReachedEndOfText) : Exit While
          Select Case Curr
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
    _res_.LastParse = Curr
    Return _res_
  End Function

  Public Function Analyse_Numeric_ToString(ct As CancellationToken, format As String, Optional Provider As IFormatProvider = Nothing) As OutputResult(Of String)
    Dim _res_ As New OutputResult(Of String)
    If format Is Nothing Then _res_.AddError(New Internal_IssueReport(New ArgumentNullException("format").ToString)) : Return _res_
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
              _res_.AddError(New UnknownSpecifier(format(0), 0))
            End If
          Case 2
            If _SNFS_.Contains(format(0)) Then
              If format(1).IsDigit Then
                ' Parsed as a standard format string.
              Else
                ' Parse as a Custom Numeric format string
                _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, Provider))
              End If
            Else
              _res_.AddError(New UnknownSpecifier(format(0), 0))
            End If
          Case 3
            If _SNFS_.Contains(format(0)) Then
              If format(1).IsDigit Then
                If format(2).IsDigit Then
                  ' Parsed as a standard format string.
                Else
                  ' Parse as a Custom Numeric format string
                  _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, Provider))
                End If
              Else
                ' Parse as a Custom Numeric format string
                _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, Provider))
              End If
            Else
              _res_.AddError(New UnknownSpecifier(format(0), 0))
            End If
          Case Else
            ' Parse as a Custom Numeric format string
            _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, Provider))
        End Select

      Else
        ' parse custon numeric string.
        _res_.IncludeErrorsFrom(Analyse_Custom_Numeric(ct, format, Provider))
      End If
    End If
    '   _res_.LastParse = ??
    Return _res_
  End Function

  Private Function Analyse_Custom_DateTime(ct As CancellationToken, format As String, Optional Provider As IFormatProvider = Nothing) As OutputResult(Of String)
    Dim _res_ As New OutputResult(Of String)
    '_res_.AddError(New Internal_Information("(DateTime) CustomFormatString Diagnostic Not yet Implemented."))
    Dim _ExitOnFirst_ = False
    Dim s As New TheSourceText(format)
    Dim Curr As New ParsedChar(s, 0)

    While Curr.IsNotEoT
      Select Case Curr.Value
        Case "d"c
          Dim reps = Curr.RepCount("d"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case 3
              Case 4
              Case 5
              Case 6
              Case 7
              Case Else
            End Select
            Curr = _res_.LastParse
          End If
        Case "f"c
          Dim reps = Curr.RepCount("f"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case 3
              Case 4
              Case 5
              Case 6
              Case 7
              Case Else
            End Select
            Curr = _res_.LastParse
          End If
        Case "F"c
          Dim reps = Curr.RepCount("F"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case 3
              Case 4
              Case 5
              Case 6
              Case 7
              Case Else
            End Select
            Curr = _res_.LastParse
          End If
        Case "g"c
          Dim reps = Curr.RepCount("g"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New SpecifierUnkown(New String("g"c, reps.Output), Curr.Index, reps.LastParse.Index))
            End Select
            Curr = _res_.LastParse
          End If
        Case "h"c
          Dim reps = Curr.RepCount("h"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case Else
                ' Add an error unknown specifier
            End Select
            Curr = _res_.LastParse
          End If
        Case "H"c
          Dim reps = Curr.RepCount("H"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New SpecifierUnkown(New String("H"c, reps.Output), Curr.Index, reps.LastParse.Index))
            End Select
            Curr = _res_.LastParse
          End If
        Case "K"c
          Dim reps = Curr.RepCount("K"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New SpecifierUnkown(New String("K"c, reps.Output), Curr.Index, reps.LastParse.Index))
            End Select
            Curr = _res_.LastParse
          End If
        Case "m"c
          Dim reps = Curr.RepCount("m"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New SpecifierUnkown(New String("m"c, reps.Output), Curr.Index, reps.LastParse.Index))
            End Select
            Curr = _res_.LastParse
          End If
        Case "M"c
          Dim reps = Curr.RepCount("M"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New SpecifierUnkown(New String("M"c, reps.Output), Curr.Index, reps.LastParse.Index))
            End Select
            Curr = _res_.LastParse
          End If
        Case "s"c
          Dim reps = Curr.RepCount("s"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New SpecifierUnkown(New String("s"c, reps.Output), Curr.Index, reps.LastParse.Index))
            End Select
            Curr = _res_.LastParse
          End If
        Case "t"c
          Dim reps = Curr.RepCount("t"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New SpecifierUnkown(New String("t"c, reps.Output), Curr.Index, reps.LastParse.Index))
            End Select
            Curr = _res_.LastParse
          End If
        Case "y"c
          Dim reps = Curr.RepCount("y"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case 3
              Case 4
              Case 5
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New SpecifierUnkown(New String("y"c, reps.Output), Curr.Index, reps.LastParse.Index))
            End Select
            Curr = _res_.LastParse
          End If
        Case "z"c
          Dim reps = Curr.RepCount("z"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            Select Case reps.Output
              Case 0 ' Should never occure
              Case 1
              Case 2
              Case 3
              Case Else
                ' Add an error unknown specifier
                _res_.AddError(New SpecifierUnkown(New String("z"c, reps.Output), Curr.Index, reps.LastParse.Index))
            End Select
            Curr = _res_.LastParse
          End If
        Case ":"c
          Dim reps = Curr.RepCount(":"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            If reps.Output <> 1 Then _res_.AddError(New SpecifierUnkown(New String(":"c, reps.Output), Curr.Index, reps.LastParse.Index))
            Curr = _res_.LastParse
          End If
        Case "/"c
          Dim reps = Curr.RepCount("/"c)
          _res_.IncludeErrorsFrom(reps)
          If reps.IsValid = False Then
            If reps.Output <> 1 Then _res_.AddError(New SpecifierUnkown(New String("/"c, reps.Output), Curr.Index, reps.LastParse.Index))
            Curr = _res_.LastParse
          End If
        Case "\"c
          Curr = Curr.Next
          If Curr.IsEoT Then _res_.AddError(New UnexpectedlyReachedEndOfText()) : Exit While
          Curr = Curr.Next
        Case "'"c, _QUOTE_
          Dim r = LiteralString(Curr, Curr.Value)
          _res_.IncludeErrorsFrom(r)
          If r.IsValid = False Then Exit While
          Curr = r.LastParse
        Case "%"c
          Dim nc = Curr.Next
          If nc.IsEoT Then
            _res_.AddError(New UnexpectedlyReachedEndOfText)
          Else
            If "dfFghHKmMstyz:/".Contains(nc.Value) Then
              Curr = nc
            Else
              _res_.AddError(New UnexpectedChar(nc.Value, nc.Index))
            End If
          End If
        Case Else
          Curr = Curr.Next
      End Select
    End While
    _res_.LastParse = Curr
    Return _res_
  End Function

  Public Function Analyse_DateTime_ToString(ct As CancellationToken, format As String, Optional Provider As IFormatProvider = Nothing) As OutputResult(Of String)
    Dim _res_ As New OutputResult(Of String)
    If format Is Nothing Then _res_.AddError(New Internal_IssueReport(New ArgumentNullException("format").ToString)) : Return _res_
    Dim cf As ICustomFormatter = Nothing
    If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
    If format.Length = 0 Then Return _res_

    If format.Length = 1 Then
      ' Standard Date and Time Format Strings (http://msdn.microsoft.com/en-us/library/az4se3k1(v=vs.110)
      If "dDfFgGmMoOrRstTuUyY".Contains(format(0)) Then
        ' Valid specifier
      Else
        _res_.AddError(New UnknownSpecifier(format(0), 0))
      End If
    Else
      ' Custom format string
      _res_.IncludeErrorsFrom(Analyse_Custom_DateTime(ct, format, Provider))
    End If
    ''    _res_.LastParse = ??
    Return _res_
  End Function

  Private Function Analyse_Custom_TimeSpan(ct As CancellationToken, format As String, Optional Provider As IFormatProvider = Nothing) As OutputResult(Of String)
    Dim _res_ As New OutputResult(Of String)
    '_res_.AddError(New Internal_Information("(TimeSpan) CustomFormatString Diagnostic Not yet Implemented."))
    Dim Curr As ParsedChar
    Const _TS_ = "dhmsfF"
    Select Case format.Length
      Case 0
      Case 1
        If _TS_.Contains(format(0)) = False Then _res_.AddError(New UnknownSpecifier(format(0), 0))
      Case 2
        If Not ((format(0) = "%"c) AndAlso _TS_.Contains(format(1))) Then
          _res_.AddError(New UnknownSpecifier(format(1), 1))
        ElseIf Not ((format(0) = " "c) AndAlso _TS_.Contains(format(1))) Then
          _res_.AddError(New UnknownSpecifier(format(1), 1))
        ElseIf Not (_TS_.Contains(format(0)) AndAlso (format(1) = " "c)) Then
          _res_.AddError(New UnknownSpecifier(format(1), 1))
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
                  Case 1 '
                  Case 2 To 8
                  Case Else
                    _res_.AddError(New SpecifierUnkown(New String("d"c, reps.Output), Curr.Index, reps.LastParse.Index))
                End Select
              End If
              Curr = reps.LastParse
            Case "h"c
              Dim reps = Curr.RepCount("h"c)
              _res_.IncludeErrorsFrom(reps)
              If reps.IsValid Then
                Select Case reps.Output
                  Case 0
                  Case 1
                  Case 2
                  Case Else
                    _res_.AddError(New SpecifierUnkown(New String("h"c, reps.Output), Curr.Index, reps.LastParse.Index))
                End Select
              End If
              Curr = reps.LastParse
            Case "m"c
              Dim reps = Curr.RepCount("m"c)
              _res_.IncludeErrorsFrom(reps)
              If reps.IsValid Then
                Select Case reps.Output
                  Case 0
                  Case 1
                  Case 2
                  Case Else
                    _res_.AddError(New SpecifierUnkown(New String("m"c, reps.Output), Curr.Index, reps.LastParse.Index))
                End Select
              End If
              Curr = reps.LastParse
            Case "s"c
              Dim reps = Curr.RepCount("s"c)
              _res_.IncludeErrorsFrom(reps)
              If reps.IsValid Then
                Select Case reps.Output
                  Case 0
                  Case 1
                  Case 2
                  Case Else
                    _res_.AddError(New SpecifierUnkown(New String("s"c, reps.Output), Curr.Index, reps.LastParse.Index))
                End Select
              End If
              Curr = reps.LastParse
            Case "f"c
              Dim reps = Curr.RepCount("f"c)
              _res_.IncludeErrorsFrom(reps)
              If reps.IsValid Then
                Select Case reps.Output
                  Case 0
                  Case 1
                  Case 2 To 7
                  Case Else
                    _res_.AddError(New SpecifierUnkown(New String("f"c, reps.Output), Curr.Index, reps.LastParse.Index))
                End Select
              End If
              Curr = reps.LastParse
            Case "F"c
              Dim reps = Curr.RepCount("F"c)
              _res_.IncludeErrorsFrom(reps)
              If reps.IsValid Then
                Select Case reps.Output
                  Case 0
                  Case 1
                  Case 2
                  Case Else
                    _res_.AddError(New SpecifierUnkown(New String("F"c, reps.Output), Curr.Index, reps.LastParse.Index))
                End Select
              End If
              Curr = reps.LastParse
            Case "'"c
              Dim r = LiteralString(Curr, Curr.Value)
              _res_.IncludeErrorsFrom(r)
              If r.IsValid = False Then Exit While
              Curr = r.LastParse
            Case "\"c
              If Curr.Next.IsEoT Then _res_.AddError(New UnexpectedlyReachedEndOfText()) : Exit While
              Curr = Curr.Next.Next
            Case Else
              ' NOTE: There is potential for this to be incorrect 
              _res_.AddError(New UnexpectedChar(Curr.Value, Curr.Index))
              Exit While
          End Select

        End While
    End Select
    _res_.LastParse = Curr
    Return _res_
  End Function

  Public Function Analyse_TimeSpan_ToString(ct As CancellationToken, format As String, Optional Provider As IFormatProvider = Nothing) As OutputResult(Of String)
    Dim _res_ As New OutputResult(Of String)
    If format Is Nothing Then _res_.AddError(New Internal_IssueReport(New ArgumentNullException("fs").ToString)) : Return _res_

    Dim cf As ICustomFormatter = Nothing
    If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
    If format.Length = 0 Then Return _res_
    If format.Length = 1 Then
      ' Standard TimeSpan Format Strings (http://msdn.microsoft.com/en-us/library/ee372286(v=vs.110)
      If "cgG".Contains(format(0)) Then
        ' Valid specifier
      Else
        _res_.AddError(New UnknownSpecifier(format(0), 0))
      End If
    Else
      ' Custom format string
      _res_.IncludeErrorsFrom(Analyse_Custom_TimeSpan(ct, format, Provider))
    End If
    '    _res_.LastParse = ??
    Return _res_
  End Function

  Public Function Analyse_DateTimeOffset_ToString(ct As CancellationToken, format As String, Optional Provider As IFormatProvider = Nothing) As OutputResult(Of String)
    Dim _res_ As New OutputResult(Of String)
    If format Is Nothing Then _res_.AddError(New Internal_IssueReport(New ArgumentNullException("fs").ToString)) : Return _res_
    Dim cf As ICustomFormatter = Nothing
    If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
    If format.Length = 0 Then Return _res_
    If format.Length = 1 Then
      ' Standard DateTimeOffset Format Strings (http://msdn.microsoft.com/en-us/library/bb346136(v=vs.110)
      If "cgGKUru".Contains(format(0)) Then
        ' Valid specifier
      Else
        _res_.AddError(New UnknownSpecifier(format(0), 0))
      End If
    Else
      ' Custom format string
      _res_.AddError(New Internal_Information("(DataTimeOffset) CustomFormatString Diagnostic Not yet Implemented."))
    End If
    '    _res_.LastParse = ??
    Return _res_
  End Function

  Public Function Analyse_Enum_ToString(ct As CancellationToken, format As String, Optional Provider As IFormatProvider = Nothing) As OutputResult(Of String)
    Dim _res_ As New OutputResult(Of String)
    If format Is Nothing Then _res_.AddError(New Internal_IssueReport(New ArgumentNullException("fs").ToString)) : Return _res_
    Dim cf As ICustomFormatter = Nothing
    If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)

    If format.Length = 0 Then Return _res_
    If format.Length = 1 Then
      ' Standard Enum Format Strings (http://msdn.microsoft.com/en-us/library//c3s1ez6e(v=vs.110)
      If "GgFfDdXx".Contains(format(0)) Then
        ' Valid specifier
        '    _res_.LastParse = ??
        Return _res_
      Else
        _res_.AddError(New UnknownSpecifier(format(0), 0))
      End If
    Else
      ' Custom format string
      _res_.AddError(New Internal_Information("(Enum) CustomFormatString Diagnostic Not yet Implemented."))
    End If
    '    _res_.LastParse = ??
    Return _res_
  End Function



  Public Function AnalyseFormatString(ct As CancellationToken, format As String, NumOfArgs As Integer,
                                         Args As IEnumerable(Of Object),
                                        Optional Provider As IFormatProvider = Nothing) As OutputResult(Of System.Text.StringBuilder)
    If format Is Nothing Then Throw New ArgumentNullException("fs")
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
    Dim _res_ As New OutputResult(Of System.Text.StringBuilder)
    _res_.Output = New System.Text.StringBuilder()



    Dim curr As New ParsedChar(New TheSourceText(format), 0)
    Dim ArgsSupplied = NumOfArgs > 0
    Dim ArgsCounted = 0
    Dim internalError As Internal_IssueReport = Nothing
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
              If (curr IsNot Nothing) AndAlso (curr.Next IsNot Nothing) AndAlso (curr.Next = Closing_Brace) Then
                ' This brace has escaped! }}
                curr = curr.Next
              Else
                If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function
                _res_.AddError(New UnexpectedChar(curr.Value, curr.Index))
                If ExitOnFirst Then GoTo Exit_Function
              End If
            Case Opening_Brace
              If (curr IsNot Nothing) AndAlso (curr.Next IsNot Nothing) AndAlso (curr.Next = Opening_Brace) Then
                ' This brace has escaped! {{
                curr = curr.Next
              Else
                If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function
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
        If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function
        ' Get current character of the text.
        'CurrentCharacter = fr.Curr.Value
        If ArgsSupplied AndAlso Not IsDigit(curr) Then
          ParsingIsInAnErrorState = True
          _res_.AddError(New UnexpectedChar(curr.Value, curr.Index))
          If ExitOnFirst Then GoTo Exit_Function
        End If
        '
        ' +---------------------------------------------------
        ' | Start Parsing for the Index value
        ' +---------------------------------------------------
        '
        ' -- Parse and Calculate IndexPart Value
        '
        ' IDEA: Use ParseDigit and Value Method that is used in ExponentValue
        '
        Dim ArgIndex = ParseValue(curr, ct, _LIMIT_, ParsingIsInAnErrorState)
        _res_.IncludeErrorsFrom(ArgIndex)
        ' Why did we exit?
        ArgsCounted += 1
        EndPositionForThisPart = curr.Index - 1
        If ArgsSupplied AndAlso ArgIndex.Output >= _LIMIT_ Then
          ' Index Value is greater or equal to limit.
          _res_.AddError(New ArgIndexHasExceedLimit("Arg Index", ArgIndex.Output.ToString, _LIMIT_, StartPositionForThisPart, EndPositionForThisPart)) ' NOTE: Check API
          InvalidIndex = True
          If ExitOnFirst Then GoTo Exit_Function
        End If
        If ArgsSupplied AndAlso Not ParsingIsInAnErrorState AndAlso (ArgIndex.Output >= NumOfArgs) Then
          ' Index is out of the bounds of the supplied args.
          _res_.AddError(New ArgIndexOutOfRange(ArgIndex.Output, NumOfArgs, StartPositionForThisPart, EndPositionForThisPart))
          ' ToDo: Get the Start and End positions of opening and closing braces.
          InvalidIndex = True
          If ExitOnFirst Then GoTo Exit_Function
        End If
        ' Reset the ParsingIsInAnErrorState Flag 
        ParsingIsInAnErrorState = False
        ConsumeSpaces(curr, ct)
        If curr Is Nothing Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function

        '
        ' +-----------------------------------------
        ' | Start of Parsing for the AlignmentPart  
        ' +-----------------------------------------
        '
        Dim LeftJustifiy = False
        If curr = _COMMA_ Then
          curr = curr.Next
          ConsumeSpaces(curr, ct)
          If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function
          'CurrentCharacter = fr.Curr.Value
          If curr = _MINUS_ Then
            LeftJustifiy = True
            curr = curr.Next
            If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function

            'CurrentCharacter = fr.Curr.Value
          End If
          ' If next Character after a minus, or currenct character isnot a Digit then it is an error
          If ArgsSupplied AndAlso Not IsDigit(curr) Then
            ParsingIsInAnErrorState = True
            _res_.AddError(New UnexpectedChar(curr.Value, curr.Index))
            If ExitOnFirst Then GoTo Exit_Function
          End If
          ' Reset the markers for highlighter
          StartPositionForThisPart = curr.Index
          EndPositionForThisPart = curr.Index
          Width = ParseValue(curr, ct, _LIMIT_, ParsingIsInAnErrorState)
          _res_.IncludeErrorsFrom(Width)
          ' Why did we exit?
          EndPositionForThisPart = curr.Index - 1
          If ArgsSupplied AndAlso Width.Output >= _LIMIT_ Then
            ' Index Value is greater or equal to limit.
            Dim WidthText = format.Substring(StartPositionForThisPart, (EndPositionForThisPart - StartPositionForThisPart) + 1)
            _res_.AddError(New ArgIndexHasExceedLimit("Value when limit was exceeded. ", WidthText, _LIMIT_, StartPositionForThisPart, EndPositionForThisPart))
            If ExitOnFirst Then GoTo Exit_Function
          End If
        End If
        ConsumeSpaces(curr, ct)
        If ArgsSupplied AndAlso Not ParsingIsInAnErrorState AndAlso (curr Is Nothing) Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function
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
            If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function
            Select Case curr.Value
              Case Opening_Brace
                If (curr IsNot Nothing) AndAlso (curr.Next IsNot Nothing) AndAlso (curr.Next = Opening_Brace) Then
                  ' This brace has escaped! {{
                  curr = curr.Next
                Else
                  If ArgsSupplied Then
                    If curr Is Nothing Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function
                    _res_.AddError(New UnexpectedlyReachedEndOfText)
                  End If

                  If ExitOnFirst Then GoTo Exit_Function
                End If
              Case Closing_Brace
                If (curr IsNot Nothing) AndAlso (curr.Next IsNot Nothing) AndAlso (curr.Next = Closing_Brace) Then
                  ' This brace has escaped! }}
                  curr = curr.Next
                Else
                  If ArgsSupplied AndAlso (curr Is Nothing) Then _res_.AddError(New UnexpectedlyReachedEndOfText) : GoTo Exit_Function
                  Exit While
                End If
            End Select
            If fmt Is Nothing Then fmt = New System.Text.StringBuilder
            fmt.Append(curr.Value)
            curr = curr.Next
          End While
        End If
        If curr.Value <> Closing_Brace Then
          If ArgsSupplied Then
            ParsingIsInAnErrorState = True
            _res_.AddError(New UnexpectedChar(curr.Value, curr.Index))
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
            s = formattableArg.ToString(sFmt, Provider)
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
      If ArgsSupplied AndAlso ArgsCounted = 0 Then _res_.AddError(New ContainsNoArgs)
      If Not (ArgsSupplied) AndAlso ArgsCounted > 0 Then _res_.AddError(New ContainsNoParameters)

    Catch ex As Exception
      ' Let's use the IDE error window to also report internal errors, :-)
      internalError = New Internal_IssueReport(ex.ToString)
    End Try
Exit_Function:
    If internalError IsNot Nothing Then _res_.AddError(internalError)
    _res_.AddError(New FinalOutput(_res_.Output.ToString))
    _res_.LastParse = curr
    Return _res_
  End Function


  Private Function ParseValue(ByRef pc As ParsedChar, ct As CancellationToken, Limit As Integer, ByRef ParsingIsInAnErrorState As Boolean) As OutputResult(Of Integer)
    Dim _res_ As New OutputResult(Of Integer)
    Do
      If ct.IsCancellationRequested Then Exit Do
      If Not ParsingIsInAnErrorState Then _res_.Output = (10 * _res_.Output) + DigitValue(pc.Value)
      pc = pc.Next
      If pc.IsEoT Then Exit Do
      If Not ParsingIsInAnErrorState AndAlso _res_.Output >= Limit Then ParsingIsInAnErrorState = True
    Loop While IsDigit(pc)
    _res_.LastParse = pc
    Return _res_
  End Function

  Private Sub ConsumeSpaces(ByRef pc As ParsedChar, ct As CancellationToken)
    ' Consume spaces
    While pc IsNot Nothing
      If ct.IsCancellationRequested Then Exit Sub
      If pc.Value <> _SPACE_ Then Exit While
      pc = pc.Next()
    End While
  End Sub
  Private Function SkipSpaces(pc As ParsedChar, ct As CancellationToken) As ParsedChar
    ' Consume spaces
    While pc IsNot Nothing
      If ct.IsCancellationRequested Then Exit While
      If pc.Value <> _SPACE_ Then Exit While
      pc = pc.Next()
    End While
    Return pc
  End Function
  'Private Function IsDigit(c As ParsedChar) As Boolean
  '  Return ("0"c <= c.Value) AndAlso (c.Value <= "9"c)
  'End Function

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



