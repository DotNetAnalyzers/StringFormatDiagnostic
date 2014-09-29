Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Errors


      Public Class UnknownSpecifier
        Inherits Error_Position
        Public Sub New(c As Char, Start As Integer)
          MyBase.New(String.Format("Unknown Specifier [{0}] at {1}.", c, Start), Start, 1)
        End Sub


      End Class

    End Namespace
  End Namespace
End Namespace
