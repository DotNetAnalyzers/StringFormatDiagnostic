Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Errors

      Public Class ArgIndexHasExceedLimit
        Inherits ValueOutOfBoundsLimit
        Public Sub New(ParamName As String, Value As Integer, Limit As Integer, start As Integer, Finish As Integer)
          MyBase.New(Value, start, Finish, ParamName, 0, Limit)
          Me._Message = String.Format("{2} of ({0}) has exceed .net String.Format limit of {1}.", Value, Limit, ParamName)
        End Sub
      End Class

    End Namespace
  End Namespace
End Namespace
