Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Errors

      Public Class ArgIndexOutOfRange
        Inherits ValueOutOfBoundsLimit
        Public Sub New(ParamName As String, Value As Integer, Limit As Integer, start As Integer, Finish As Integer)
          MyBase.New(Value, start, Finish, ParamName, 0, Limit)
          Me._Message = String.Format("Index of ({0}) is invalid. (0 <= Index < {1})", Value, Limit)
        End Sub
      End Class

    End Namespace
  End Namespace
End Namespace
