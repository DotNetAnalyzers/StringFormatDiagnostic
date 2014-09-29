Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports._Internal


      Public Class Warning
        Inherits IssueReports.Warning
        Sub New(Msg As String)
          MyBase.New("(Internal Warning) " & Msg)
        End Sub
      End Class

    End Namespace
  End Namespace
End Namespace
