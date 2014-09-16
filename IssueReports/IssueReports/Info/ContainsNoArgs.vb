Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Info


      Public Class ContainsNoArgs
        Inherits Information
        Public Sub New()
          MyBase.New("Contains No Args")
        End Sub

        Shared Private ReadOnly  _Default_ As ContainsNoArgs = New ContainsNoArgs()
        Public Shared Function [Default]() As ContainsNoArgs
          Return _Default_ 
        End Function

      End Class

    End Namespace
  End Namespace
End Namespace
