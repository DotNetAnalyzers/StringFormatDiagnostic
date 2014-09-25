Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Warnings


      Public Class IgnoredChar
        Inherits Warning_Position
        Public Sub New(c As Char, Start As Integer)
          MyBase.New(String.Format("Ignored Char [{0}] at {1}.", c, Start), Start, 1)
        End Sub

        'Shared Private ReadOnly  _Default_ As ContainsNoArgs = New ContainsNoArgs()
        'Public Shared Function [Default]() As ContainsNoArgs
        '  Return _Default_ 
        'End Function

      End Class

    End Namespace
  End Namespace
End Namespace
