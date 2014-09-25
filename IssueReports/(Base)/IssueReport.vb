Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics

Namespace Global.AdamSpeight2008

  Namespace StringFormatDiagnostic

    Namespace IssueReports


      Public MustInherit Class IssueReport
        Implements IReportIssue
        Friend _Message As String = String.Empty
        Friend _Level As DiagnosticSeverity = DiagnosticSeverity.Error

        Public ReadOnly Property Level As DiagnosticSeverity Implements IReportIssue.Level
          Get
            Return _Level
          End Get
        End Property
        Public ReadOnly Property Message As String Implements IReportIssue.Message
          Get
            Return _Message
          End Get
        End Property

        Friend Sub New(Level As DiagnosticSeverity, Msg As String)
          _Level = Level
          _Message = If(Msg, String.Empty)
        End Sub
      End Class

      Public MustInherit Class IssueReportWithStartPosition
        Inherits IssueReport
        Implements IReportIssueWithPositionAndLength

        Friend _Index  As Integer = 0
        Friend _Length As Integer = 0


        Public ReadOnly Property Index As Integer Implements IReportIssueWithPositionAndLength.Index
          Get
            Return _Index
          End Get
        End Property

        Public ReadOnly Property Length As Integer Implements IReportIssueWithPositionAndLength.Length
          Get
            Return _Length
          End Get
        End Property

        Friend Sub New(Level As DiagnosticSeverity, Msg As String, Start As Integer, Optional Length As Integer = 1)
          MyBase.New(Level, Msg)
          _Index = Start
          _Length = Length
        End Sub
      End Class

      Public MustInherit Class Information
        Inherits IssueReport
        Public Sub New(Msg As String)
          MyBase.New(DiagnosticSeverity.Info, Msg)
        End Sub
      End Class

      Public MustInherit Class [Error]
        Inherits IssueReport
        Public Sub New(Msg As String)
          MyBase.New(DiagnosticSeverity.Error, Msg)
        End Sub
      End Class

      Public MustInherit Class Hidden
        Inherits IssueReport
        Public Sub New(Msg As String)
          MyBase.New(DiagnosticSeverity.Hidden, Msg)
        End Sub
      End Class

      Public MustInherit Class Warning
        Inherits IssueReport
        Public Sub New(Msg As String)
          MyBase.New(DiagnosticSeverity.Warning, Msg)
        End Sub
      End Class

      Public MustInherit Class Information_Position
        Inherits IssueReportWithStartPosition
        Public Sub New(Msg As String, Start As Integer, Optional Length As Integer = 0)
          MyBase.New(DiagnosticSeverity.Info, Msg, Start, Length)
        End Sub
      End Class

      Public MustInherit Class Error_Position
        Inherits IssueReportWithStartPosition
        Public Sub New(Msg As String, Start As Integer, Optional Length As Integer = 0)
          MyBase.New(DiagnosticSeverity.Error, Msg, Start, Length)
        End Sub
      End Class

      Public MustInherit Class Hidden_Position
        Inherits IssueReportWithStartPosition
        Public Sub New(Msg As String, Start As Integer, Optional Length As Integer = 0)
          MyBase.New(DiagnosticSeverity.Hidden, Msg, Start, Length)
        End Sub
      End Class

      Public MustInherit Class Warning_Position
        Inherits IssueReportWithStartPosition
        Public Sub New(Msg As String, Start As Integer, Optional Length As Integer = 0)
          MyBase.New(DiagnosticSeverity.Warning, Msg, Start, Length)
        End Sub
      End Class

    End Namespace

  End Namespace

End Namespace


