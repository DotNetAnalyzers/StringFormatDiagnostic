Imports System.Threading
Imports Analysers
Imports SFD.StringFormat
Imports SFD.StringFormat.StringFormat

Namespace Global.SFD.Analysis

  Public Class [Enum]
    Implements IDiagnosticAnalyser

    Public Function Analyse(ct As CancellationToken, span As SpanKind, text As String, FormatIndex As Integer, provider As IFormatProvider, Args As IEnumerable(Of Object)) As IEnumerable(Of Base_Issue) Implements IDiagnosticAnalyser.Analyse
      Dim Issues As New List(Of Issues.Base_Issue)
      If text Is Nothing Then Return Issues '.AddError(New _Internal.Warning(New ArgumentNullException("fs").ToString)) : Return _res_
      Dim cf As ICustomFormatter = Nothing
      If provider IsNot Nothing Then cf = CType(provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      If text.Length = 0 Then Return Issues
      If text.Length = 1 Then
        ' Standard Enum Format Strings (http://msdn.microsoft.com/en-us/library//c3s1ez6e(v=vs.110)
        If "GgFfDdXx".Contains(text(0)) Then Return Issues ' Valid specifier
        Issues.Add(New Issues.UnknownSpecifier(span.Offset(0, 1, Kinds.Err_UnknownSpecifier), text(0)))
      Else
        ' Custom format string
        ' _res_.AddError(New _Internal.Information("(Enum) CustomFormatString Diagnostic Not yet Implemented."))
      End If
      '    _res_.LastParse = ??
      Return Issues
    End Function

  End Class
End Namespace
