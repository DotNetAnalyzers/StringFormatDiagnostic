Public Class DiagMeth
  Public ReadOnly Property TypeName As String
  Public ReadOnly Property MethodNames As String()

  Public Sub New(TypeName As String, MethodNames As String())
    _TypeName = TypeName
    _MethodNames = MethodNames
  End sub

End Class
