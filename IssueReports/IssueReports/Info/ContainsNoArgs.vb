Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Info


      Public Class ContainsNoArgs
        Inherits Information
        Public Sub New()
          MyBase.New("No Args Holes were found in format string but parameter args where supplied. Is this correct?")
        End Sub

        Shared Private ReadOnly  _Default_ As ContainsNoArgs = New ContainsNoArgs()
        Public Shared Function [Default]() As ContainsNoArgs
          Return _Default_ 
        End Function

      End Class

    End Namespace
  End Namespace
End Namespace
