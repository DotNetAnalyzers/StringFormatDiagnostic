Imports Microsoft.CodeAnalysis

Namespace Global.AdamSpeight2008

  Namespace StringFormatDiagnostic

    Namespace Interfaces

      Public Interface IReportIssue
        ReadOnly Property Level() As DiagnosticSeverity
        ReadOnly Property Message() As String
      End Interface

      Public Interface IReportIssueWithPositionAndLength
        Inherits IReportIssue
        ReadOnly Property Index() As Integer
        ReadOnly Property Length() As Integer
      End Interface

    End Namespace

  End Namespace

End Namespace
