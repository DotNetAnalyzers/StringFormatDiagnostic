Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008
  Namespace StringFormatDiagnostic
    Namespace IssueReports.Errors


      Public Class SpecifierUnknown
        Inherits Error_Position
        Public Sub New(specifier As String, Start As Integer)
          MyBase.New(String.Format("Specifier Unknown [{0}] at {1}.", specifier, Start), Start, specifier.Length)
        End Sub


      End Class

    End Namespace
  End Namespace
End Namespace
