Public MustInherit Class Result
  Public ReadOnly Property Valid As Boolean

  Public ReadOnly Property [End] As ParsedChar
  Public ReadOnly Property Begin As ParsedChar


  Friend Sub New(Valid As Boolean, Begin As ParsedChar, [End] As ParsedChar)
    _Valid = Valid
    _End = [End]
    _Begin = Begin
  End Sub
  Public Overrides Function ToString() As String
    Return String.Format("{0}[{1}…{2}]", If(Valid, "√"c, "X"c), If(Begin Is Nothing, "?", Begin.Index.ToString), If([End] Is Nothing, "?", [End].Index.ToString))
  End Function
End Class

Public Class Valid(Of T)
  Inherits Result
  Public ReadOnly Property Result As T
  Public Sub New(Result As T, Begin As ParsedChar, [End] As ParsedChar)
    MyBase.New(True, Begin, [End])
    _Result = Result
  End Sub

  Public Overrides Function ToString() As String
    Return String.Format("{0} := [{1}]", MyBase.ToString, Result)
  End Function


End Class
Public Class InvalidRes
  Inherits Result
  Public ReadOnly Property TErrorMsg As String
  Public Sub New(AErrorMsg As String, Begin As ParsedChar, [End] As ParsedChar)
    MyBase.New(False, Begin, [End])
    _TErrorMsg = AErrorMsg
  End Sub

  Public Overrides Function ToString() As String
    Return String.Format("{0} Error: {1} ", MyBase.ToString(), TErrorMsg)
  End Function

End Class