Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports._Internal
        Public Class Information
          Inherits IssueReports.Information 
          Sub New(Msg As String)
          MyBase.New("(Internal Info) " & Msg)
        End Sub
        End Class



    End Namespace
  End Namespace
End Namespace
