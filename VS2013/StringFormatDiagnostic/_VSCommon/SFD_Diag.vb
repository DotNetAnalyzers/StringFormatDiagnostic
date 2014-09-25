Namespace Global.AdamSpeight2008.StringFormatDiagnostic

  Public Class SFD_Diag
    Public ReadOnly Property TypeName As String = ""
    Public ReadOnly Property MethodName As String = ""
    Public ReadOnly Property FIndex As Integer
    Public ReadOnly Property Analyser As String = ""
    Public ReadOnly Property ParamTypes As String() = {}

    Public Sub New(TypeName As String, MethodName As String, FIndex As Integer, Analyser As String, ParamTypes As String())
      _TypeName = If(TypeName, "")
      _MethodName = If(MethodName, "")
      _FIndex = FIndex
      _Analyser = If(Analyser, "")
      _ParamTypes = If(ParamTypes, {})
    End Sub
  End Class

End Namespace