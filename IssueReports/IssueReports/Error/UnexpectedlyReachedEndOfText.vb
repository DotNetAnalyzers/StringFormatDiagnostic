Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Errors

      Public Class UnexpectedlyReachedEndOfText
        Inherits Information

        Sub New()
          MyBase.New("Unexpectedly reached end of text.")
        End Sub
        Private Shared ReadOnly _Default_ As UnexpectedlyReachedEndOfText = New UnexpectedlyReachedEndOfText()
        Public Shared Function [Default]() As UnexpectedlyReachedEndOfText
          Return _Default_
        End Function
      End Class

    End Namespace
  End Namespace
End Namespace
