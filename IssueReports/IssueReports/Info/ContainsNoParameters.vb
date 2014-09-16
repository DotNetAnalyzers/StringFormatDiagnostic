Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Info

      Public Class ContainsNoParameters
        Inherits Information
        Public Sub New()
          MyBase.New("Contains No Parameters")
        End Sub
        Private Shared ReadOnly _Default_ As ContainsNoParameters = New ContainsNoParameters()
        Public Shared Function [Default]() As ContainsNoParameters
          Return _Default_
        End Function
      End Class

    End Namespace
  End Namespace
End Namespace
