Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Warnings


      Public Class TooManySections
        Inherits Warning_Position
        Public Sub New(Index As Integer)
          MyBase.New("Too Many Sections", Index)
        End Sub

      End Class

    End Namespace
  End Namespace
End Namespace
