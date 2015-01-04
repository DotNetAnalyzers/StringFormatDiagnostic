Imports System.Threading
Imports Analysers
Imports SFD.Analysis
Imports SFD.StringFormat
Imports SFD.StringFormat.StringFormat

Namespace Global.SFD.Analysis

  Public Class DateTime
    Implements IDiagnosticAnalyser


    Public Function Analyse(ct As CancellationToken, span As SpanKind, format As String, FormatIndex As Integer, provider As IFormatProvider, Args As IEnumerable(Of Object)) As IEnumerable(Of Base_Issue) Implements IDiagnosticAnalyser.Analyse
      Dim Issues As New List(Of Issues.Base_Issue)
      If format Is Nothing Then Return Issues '.AddError(New _Internal.Warning(New ArgumentNullException("format").ToString)) : Return _res_
      Dim cf As ICustomFormatter = Nothing
      If provider IsNot Nothing Then cf = CType(provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      If format.Length = 0 Then Return Issues

      If format.Length = 1 Then
        ' Standard Date and Time Format Strings (http://msdn.microsoft.com/en-us/library/az4se3k1(v=vs.110)
        If "dDfFgGmMoOrRstTuUyY".Contains(format(0)) Then
          ' Valid specifier
        Else
          Issues.Add(New Issues.UnknownSpecifier(span.Offset(0, 1, StringFormat.StringFormat.Kinds.Err_UnknownSpecifier), format(0)))
        End If
      Else
        ' Custom format string
        Issues.AddRange(Analyse_Custom_DateTime(ct, format, span, provider, Args))
      End If
      '    _res_.LastParse = ??
      Return Issues
    End Function

    Function RepCount(sr As SourceText, ByRef i As Integer, c As Char) As Integer
      Dim reps = 0
      While i < sr.Length
        If sr(i).HasValue = False Then Exit While
        If sr(i).Value = c Then
          reps += 1
          i += 1
        Else
          Exit While
        End If
      End While
      Return reps
    End Function

    Private Function Analyse_Custom_DateTime(ct As CancellationToken, format As String, span As SpanKind, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As List(Of Issues.Base_Issue)
      Dim Issues As New List(Of Issues.Base_Issue)
      If format Is Nothing Then Return Issues 
      Dim _ExitOnFirst_ = False
      Dim sr = SourceText.Create(format)
      Dim i = 0
      While i < sr.Length
        Select Case sr(i)
          Case "d"c
            Dim reps = RepCount(sr, i, "d"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1 To 7
              Case Else
            End Select
          Case "f"c
            Dim reps = RepCount(sr, i, "f"c) 'Curr.RepCount("f"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1 To 7
              Case Else
            End Select
          Case "F"c
            Dim reps = RepCount(sr, i, "F"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1 To 7
              Case Else
            End Select
          Case "g"c
            Dim si = i
            Dim reps = RepCount(sr, i, "g"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("g"c, reps)))
            End Select
          Case "h"c
            Dim si = i
            Dim reps = RepCount(sr, i, "h"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("h"c, reps)))
            End Select
       Case "H"c
            Dim si = i
            Dim reps = RepCount(sr,i,"H"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("H"c, reps)))
            End Select
         Case "K"c
            Dim si =i
            Dim reps = RepCount(sr,i,"K"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("K"c, reps)))
            End Select
         Case "m"c
            Dim si = i
            Dim reps = RepCount(sr,i,"m"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("m"c, reps)))
            End Select
        Case "M"c
            Dim si = i
            Dim reps = RepCount(sr,i,"M"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                 ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("M"c, reps)))
            End Select
          Case "s"c
            Dim si = i
            Dim reps = RepCount(sr,i,"s"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("s"c, reps)))
            End Select
          Case "t"c
            Dim si = i
            Dim reps = RepCount(sr,i,"t"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1, 2
              Case Else
                ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("t"c, reps)))
            End Select
          Case "y"c
            Dim si = i
            Dim reps = RepCount(sr,i,"y"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1 To 5
              Case Else
                ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("y"c, reps)))
            End Select
          Case "z"c
            Dim si = i
            Dim reps = RepCount(sr, i, "z"c)
            Select Case reps
              Case 0 ' Should never occure
              Case 1, 2, 3
              Case Else
                ' Add an error unknown specifier
                Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("z"c, reps)))
            End Select
          Case ":"c
            Dim si = i
            Dim reps = RepCount(sr,i,":"c)
            If reps <> 1 Then Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String(":"c, reps)))
          Case "/"c
            Dim si = i
            Dim reps = RepCount(sr, i, "/"c)
            If reps <> 1 Then Issues.Add(New SpecifierUnknown(span.Offset(si, i, Kinds.Err_SpecifierUnknown), New String("/"c, reps)))
          Case "\"c
            i+=1
            If i >= sr.Length Then Issues.Add( New UnexpectedEoT(span.Offset(i,i+1,Kinds.Err_EOT)) ) : Return Issues 
          Case "'"c, """"c
            LiteralString(sr,i,issues)
''            _res_.AddRange(r)
'            If r.Value = False Then Exit While
''            Curr = r.SR
          Case "%"c
            Dim nc = sr(i+1)
            If nc.HasValue Then Issues.Add(New UnexpectedEoT(span.Offset(i, i + 1, Kinds.Err_EOT))) : Return Issues
            i+=1
            If "dfFghHKmMstyz:/".Contains(nc.Value) Then
              'Curr.Next()
            Else
              Issues.Add( New Issues.Err_UC(span.Offset(i,i+1,Kinds.Err_UC),nc.Value)) 
            End If
          Case Else
        End Select
        i+=1
      End While
      Return Issues
    End Function
    Private Sub LiteralString(sr As SourceText, i As Integer, issues As List(Of Base_Issue))
      '      Dim first = sr(i)
      While i < sr.Length
        Select Case sr(i)
          Case "'"c : i += 1 : Exit While
          Case """"c : i += 1 : Exit While
            i += 1
        End Select
      End While
    End Sub

  End Class
End Namespace