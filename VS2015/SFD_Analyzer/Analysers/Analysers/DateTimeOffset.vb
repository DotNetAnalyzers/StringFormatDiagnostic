Imports System.Threading
Imports Analysers
Imports SFD.StringFormat
Imports SFD.StringFormat.StringFormat

Namespace Global.SFD.Analysis

  Public Class DateTimeOffset
    Implements IDiagnosticAnalyser

    Public Function Analyse(ct As CancellationToken, span As SpanKind, text As String, FormatIndex As Integer, provider As IFormatProvider, Args As IEnumerable(Of Object)) As IEnumerable(Of Base_Issue) Implements IDiagnosticAnalyser.Analyse
      Dim Issues As New List(Of Issues.Base_Issue)
      If text Is Nothing Then Return Issues '.AddError(New _Internal.Warning(New ArgumentNullException("fs").ToString)) : Return _res_
      Dim cf As ICustomFormatter = Nothing
      If provider IsNot Nothing Then cf = CType(provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      If text.Length = 0 Then Return Issues
      If text.Length = 1 Then
        ' Standard DateTimeOffset Format Strings (http://msdn.microsoft.com/en-us/library/bb346136(v=vs.110)
        If "cgGKUru".Contains(text(0)) Then
          ' Valid specifier
        Else
          Issues.Add(New  UnknownSpecifier(span.Offset(0, 1, Kinds.Err_UnknownSpecifier), text(0)))
        End If 
      Else
        ' Custom format string
        '_res_.AddError(New _Internal.Information("(DataTimeOffset) CustomFormatString Diagnostic Not yet Implemented."))
      End If
      '    _res_.LastParse = ??
      Return Issues
    End Function

  End Class
End Namespace

