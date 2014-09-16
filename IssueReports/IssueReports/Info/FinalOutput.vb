Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Info


      Public Class FinalOutput
        Inherits Information
        Public Sub New(output As String)
          MyBase.New(String.Format("Output:= {0}", If(output, String.Empty)))
        End Sub
      End Class

    End Namespace
  End Namespace
End Namespace
