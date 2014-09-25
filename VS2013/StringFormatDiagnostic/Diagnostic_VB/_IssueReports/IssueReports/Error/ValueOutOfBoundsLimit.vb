Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Errors


      Public Class ValueOutOfBoundsLimit
        Inherits Error_Position
        Public Sub New(Value As Integer,
                       Start As Integer,
                       Finish As Integer, ValueName As String, LowerLimit As Integer, UpperLimit As Integer)
          MyBase.New(String.Format("{0} is out the limits of the bounds {1} <= {2} <= {3} ", ValueName,
                                                                                             LowerLimit,
                                                                                                  Value,
                                                                                             UpperLimit), Start, (Finish - Start) + 1)
        End Sub

      End Class

    End Namespace
  End Namespace
End Namespace
