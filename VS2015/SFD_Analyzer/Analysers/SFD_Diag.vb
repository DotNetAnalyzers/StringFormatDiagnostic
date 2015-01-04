
Namespace Global.SFD.Analysis

Public Class SFD_Diag
  Public ReadOnly Property TypeName As String = ""
  Public ReadOnly Property MethodName As String = ""
  
    Public ReadOnly Property Coloriser As String =""
  Public ReadOnly Property FIndex As Integer
  Public ReadOnly Property Analyser As String = ""
  Public ReadOnly Property ParamTypes As String() = {}

    Public Sub New(TypeName$, MethodName$, FIndex%, Analyser$, ParamTypes$(), Coloriser$)
      _TypeName = If(TypeName, "")
      _MethodName = If(MethodName, "")
      _FIndex = FIndex
      _Analyser = If(Analyser, "")
      _ParamTypes = If(ParamTypes, {})
      _Coloriser = If(Coloriser, "")
    End Sub

  End Class

  End Namespace