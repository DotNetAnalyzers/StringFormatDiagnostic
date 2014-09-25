Option Strict On
Imports System.Threading
Imports AdamSpeight2008.StringFormatDiagnostic.IssueReports

Namespace Global.AdamSpeight2008.StringFormatDiagnostic.Analysers

  <HideModuleName>
  Public Module [Enum]

    Public Function Analyse_Enum_ToString(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As OutputResult(Of String)
      Dim _res_ As New OutputResult(Of String)
      If format Is Nothing Then _res_.AddError(New _Internal.Warning(New ArgumentNullException("fs").ToString)) : Return _res_
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
          _res_.AddError(New Errors.UnknownSpecifier(format(0), IndexOffset + 0))
        End If
      Else
        ' Custom format string
        _res_.AddError(New _Internal.Information("(Enum) CustomFormatString Diagnostic Not yet Implemented."))
      End If
      '    _res_.LastParse = ??
      Return _res_
    End Function

  End Module
End Namespace
