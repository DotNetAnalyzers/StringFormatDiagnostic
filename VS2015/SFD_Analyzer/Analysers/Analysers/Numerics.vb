Imports System.Threading
Imports Analysers
Imports SFD.StringFormat
Imports SFD.StringFormat.StringFormat

Namespace Global.SFD.Analysis

  Public Class Numerics
    Implements IDiagnosticAnalyser

    Public Function Analyse(ct As CancellationToken,
                            span As SpanKind,
                            text As String,
                            FormatIndex As Integer,
                            provider As IFormatProvider,
                            Args As IEnumerable(Of Object)
                            ) As IEnumerable(Of Base_Issue) Implements IDiagnosticAnalyser.Analyse
      Return Analyse_Numeric(span, text, provider)
    End Function


    Function MakeUnknownSpecifier(sk As SpanKind, value As Char, s As Integer, f As Integer) As UnknownSpecifier
      Dim r = sk.Offset(New Span(sk.GetSourceText, s, f), Kinds.Err_UnknownSpecifier)
      Return New UnknownSpecifier(r, value)
    End Function


    Function Analyse_Numeric(span As SpanKind, format As String, provider As IFormatProvider) As IEnumerable(Of Base_Issue)
      Dim issues As New List(Of Base_Issue) : If format Is Nothing Then Return issues
      Dim cf As ICustomFormatter = Nothing
      If provider IsNot Nothing Then cf = CType(provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)

      Dim TheSource = SourceText.Create(format)
      If TheSource.Length > 0 Then
        If format.ContainsMoreThan(1, Function(c) Char.IsLetter(c) OrElse Char.IsWhiteSpace(c)) = False Then
          Const _SNFS_ = "CcDdEeFfGgNnPpRrXx"
          Select Case format.Length
            Case 0
            Case 1
              If _SNFS_.Contains(format(0)) Then
                ' ' Parsed as a standard format string.
              Else
                issues.Add(MakeUnknownSpecifier(span, TheSource(0).Value, 0, 1))
              End If
            Case 2
              If _SNFS_.Contains(format(0)) Then
                If Char.IsDigit(format(1)) Then
                  ' Parsed as a standard format string.
                Else
                  ' Parse as a Custom Numeric format string
                  issues.AddRange(Analyse_CustomNumeric(span, format))
                  '                  _res_.IncludeFrom(Analyse_Custom_Numeric(ct, format, IndexOffset, Provider, Args))
                End If
              Else
                issues.Add(MakeUnknownSpecifier(span, TheSource(0).Value, 0, 1))

              End If
            Case 3
              If _SNFS_.Contains(format(0)) Then
                If Char.IsDigit(format(1)) Then
                  If Char.IsDigit(format(2)) Then
                    ' Parsed as a standard format string.
                  Else
                    ' Parse as a Custom Numeric format string
                    issues.AddRange(Analyse_CustomNumeric(span, format)) 'ct, IndexOffset, provider, Args))
                  End If
                Else
                  ' Parse as a Custom Numeric format string
                  issues.AddRange(Analyse_CustomNumeric(span, format)) 'ct, format, IndexOffset, provider, Args))
                End If
              Else
                issues.Add(MakeUnknownSpecifier(span, TheSource(0).Value, 0, 1))
              End If
            Case Else
              ' Parse as a Custom Numeric format string
              issues.AddRange(Analyse_CustomNumeric(span, format)) 'ct, format, IndexOffset, provider, Args))
          End Select

        Else
          ' parse custon numeric string.
          issues.AddRange(Analyse_CustomNumeric(span, format)) 'ct, format, IndexOffset, provider, Args))
        End If
      End If
      Return issues
    End Function

    Function Analyse_CustomNumeric(span As SpanKind, text As String) As IEnumerable(Of Base_Issue)
      Dim sr = SourceText.Create(text)
      Dim issues As New List(Of Base_Issue)
      Dim i = 0
      Dim Sections = 0
      Dim DecimalPoints = 0
      While i < sr.Length
        Select Case sr(i)
          Case "0"c ' Zero Placholder
          Case "#"c ' Digit Placeholder
          Case "."c  ' Decimal Point
            If DecimalPoints > 0 Then issues.Add(New IgnoredChar(span.Offset( i, i + 1,Kinds.Err_IgnoredChar), sr(i).Value))
            DecimalPoints += 1
          Case "%"c  ' Percentage Placholder
          Case "‰"c  ' Per Mille Placeholder
          Case "E"c, "e"c  ' Exponent Holder
            Analyse_Exponent(sr, i, issues, span)
          '            issues.AddRange( res )
          Case "'"c, """"c  ' Literal String Delimiter
            Analyse_LiteralString(sr, i, issues)
          Case ";"c  ' Group Separator and Number Scaling
            If Sections > 3 Then issues.Add(New TooManySections(span.Offset(i,i+1,Kinds.Err_TooManySections)))
            Sections += 1
          Case "\"c ' Escape Character
            i += 1
            If i >= sr.Length Then issues.Add(New UnexpectedEoT(span.Offset(i,i,Kinds.Err_EOT)))
            Select Case sr(i)
              Case "\"c, "0"c, "#"c, "."c, "'"c, """"c, ";"c, "%"c, "‰"c
              Case Else
                ' To Check: Could be parse error
            End Select
        End Select
        i += 1
      End While
      Return issues
    End Function

    Private Sub Analyse_LiteralString(sr As SourceText, i As Integer, issues As List(Of Base_Issue))
      While i < sr.Length
        Select Case sr(i)
          Case "'"c : i += 1 : Exit While
          Case """"c : i += 1 : Exit While
            i += 1
        End Select
      End While
    End Sub

    Function Parse_Digits(sr As SourceText, ByRef i As Integer) As Span
      Dim si = i
      While i < sr.Length
        Select Case sr(i)
          Case "0"c To "9"c : i += 1
          Case Else
            Exit While
        End Select
      End While
         Return New Span(sr, si, i)
   End Function

    Function ExponentValue(sr As SourceText, ByRef i As Integer, ByRef issues As List(Of Base_Issue),sk As SpanKind) As Span
      Dim pr = Parse_Digits(sr, i)
      If pr.Size > 0 Then
        Dim value = 0
        If Integer.TryParse(sr.GetText(pr), value) Then
          If (0 <= value) AndAlso (value < 100) Then Return pr
          issues.Add(New ValueHasExceededLimit(sk.Offset(pr.Start, pr.Finish, Kinds.Err_ValueExceedsLimit), value, 100, "Exponent"))
        End If
      End If
      Return pr
    End Function

    Function Exponent(sr As SourceText, ByRef i As Integer, ByRef issues As List(Of Base_Issue),sk As SpanKind) As Boolean
      If i >= sr.Length Then issues.Add(New SFD.Analysis.UnexpectedEoT(sk.Offset(i,i,Kinds.Err_EOT))) : Return True
      Select Case sr(i)
        Case "0"c To "9"c
          Dim pr = ExponentValue(sr, i, issues,sk)
          i = pr.Finish
        Case Else
          issues.Add(New Err_UC(sk.Offset(i, i+1,Kinds.Err_UC),sr(i).Value)) : Return True
      End Select
      Return False
    End Function

    Sub Analyse_Exponent(sr As SourceText, ByRef i As Integer, ByRef issues As List(Of Base_Issue), sk As SpanKind)
      i += 1
      Dim si = i
      Select Case sr(i)
        Case "0"c To "9"c
          Dim pr = ExponentValue(sr, i, issues, sk)
          i = pr.Finish
        Case "-"c : i += 1 : If Exponent(sr, i, issues, sk) Then Return
        Case "+"c : i += 1 : If Exponent(sr, i, issues, sk) Then Return
        Case Else
          issues.Add(New Err_UC(sk.Offset(i, i + 1, Kinds.Err_UC), sr(i).Value)) : Return
      End Select
    End Sub

  End Class

  Module Exts

    Public Enum lusive As Integer
      Inc = 1
      Exc = 0
    End Enum
    <Extension>
    Function IsBetween(Of T As IComparable(Of T))(value As T,
                                                 lowerValue As T,
                                                 upperValue As T,
                                                 Optional lclus As lusive = lusive.Inc,
                                                 Optional uclus As lusive = lusive.Exc
                                                 ) As Boolean
      Return (lowerValue.CompareTo(value) <= lclus) AndAlso (value.CompareTo(upperValue) <= uclus)
    End Function


    <Extension>
    Public Function ContainsMoreThan(fs As String, NoMoreThan As Integer, pred As Func(Of Char, Boolean)) As Boolean
      If fs Is Nothing OrElse pred Is Nothing Then Return False
      Dim count, index As Integer
      While index < fs.Length
        If pred(fs(index)) Then
          count += 1
          If count > NoMoreThan Then Return True
        End If
        index += 1
      End While
      Return False
    End Function
  End Module
End Namespace


