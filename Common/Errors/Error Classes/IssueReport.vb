Public MustInherit Class IssueReportWithStartPosition
  Inherits IssueReport
  Public ReadOnly Property Start As Integer
  Friend Sub New(Msg As String, Start As Integer)
    MyBase.New(Msg)
    _Start = Start
  End Sub
End Class

Public Class Internal_IssueReport
  Inherits IssueReport
  Sub New(Msg As String)
    MyBase.New(Msg)
  End Sub
End Class
Public Class Internal_Information
  Inherits IssueReport
  Sub New(Msg As String)
    MyBase.New(Msg)
  End Sub
  End Class


  Public MustInherit Class IssueReport
  Public ReadOnly Property Message As String
  Friend Sub New(Msg As String)
    _Message = Msg
  End Sub

End Class

#Region "Error Classes"

Public Class UnexpectedlyReachedEndOfText
  Inherits IssueReport
  Public Sub New()
    MyBase.New("Unexpectedly Reached End Of Text")
  End Sub
End Class

Public Class ArgIndexHasExceedLimit
  Inherits IssueReportWithStartPosition
  Public ReadOnly Property Finish As Integer
  Public Sub New(ParamName As String, Value As String, Limit As Integer, start As Integer, Finish As Integer)
    MyBase.New(String.Format("{2} of ({0}) has exceed .net String.Format limit of {1}.", Value, Limit, ParamName), start)
    _Finish = Finish
  End Sub
End Class

Public Class ArgIndexOutOfRange
  Inherits IssueReportWithStartPosition
  Public ReadOnly Property Finish As Integer
  Public Sub New(Index As Integer, Limit As Integer, start As Integer, Finish As Integer)
    MyBase.New(String.Format("Index of ({0}) is invalid. (0 <= Index < {1})", Index, Limit), start)
    _Finish = Finish
  End Sub
End Class
Public Class TooManySections
  Inherits IssueReportWithStartPosition
  Public ReadOnly Property Finish As Integer
  Public Sub New(Index As Integer)
    MyBase.New(String.Format("Too Many Sections"),Index)
  End Sub
End Class
Public Class UnexpectedChar
  Inherits IssueReportWithStartPosition
  Public Sub New(C As Char, Start As Integer)
    MyBase.New("Unexpected Character '" & C & "'", Start)
  End Sub
End Class
Public Class IgnoredChar
  Inherits IssueReportWithStartPosition
  Public Sub New(C As Char, Start As Integer)
    MyBase.New("Ignored Character '" & C & "'", Start)
  End Sub
End Class
Public Class UnknownSpecifier
  Inherits IssueReportWithStartPosition
  Public Sub New(C As Char, Start As Integer)
    MyBase.New("Unknown Specifier '" & C & "'", Start)
  End Sub
End Class
Public Class ContainsNoArgs
  Inherits IssueReport
  Public Sub New()
    MyBase.New("")
  End Sub
End Class

Public Class ContainsNoParameters
  Inherits IssueReport
  Public Sub New()
    MyBase.New("")
  End Sub
End Class

Public Class FinalOutput
  Inherits IssueReport
  Public Sub New(output As String)
    MyBase.New(String.Format("Output:= {0}", output))
  End Sub
End Class



#End Region