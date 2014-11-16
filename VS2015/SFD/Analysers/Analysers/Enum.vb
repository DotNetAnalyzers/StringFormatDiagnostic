Option Strict On
Imports System.Threading
Imports AdamSpeight2008.StringFormatDiagnostics.Results

Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Analysers

  <HideModuleName>
  Public Module [Enum]

    Public Function Analyse_Enum_ToString(ct As CancellationToken, format As String, IndexOffset As Integer, Provider As IFormatProvider, Args As IEnumerable(Of Object)) As Base_Result
      Dim _res_ As New Result(Of String)("")
      If format Is Nothing Then Return _res_ '.AddError(New _Internal.Warning(New ArgumentNullException("fs").ToString)) : Return _res_
      Dim cf As ICustomFormatter = Nothing
      If Provider IsNot Nothing Then cf = CType(Provider.GetFormat(GetType(ICustomFormatter)), ICustomFormatter)
      If format.Length = 0 Then Return _res_
      If format.Length = 1 Then
        ' Standard Enum Format Strings (http://msdn.microsoft.com/en-us/library//c3s1ez6e(v=vs.110)
        If "GgFfDdXx".Contains(format(0)) Then Return _res_ ' Valid specifier
        _res_.AddError(New Errors.UnknownSpecifier(IndexOffset + 0, format(0)))
      Else
        ' Custom format string
        ' _res_.AddError(New _Internal.Information("(Enum) CustomFormatString Diagnostic Not yet Implemented."))
      End If
      '    _res_.LastParse = ??
      Return _res_
    End Function

  End Module
End Namespace
