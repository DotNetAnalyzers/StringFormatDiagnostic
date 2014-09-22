
Namespace Global.AdamSpeight2008.StringFormatDiagnostic
  Namespace Interfaces

    Public Interface IFormatDiagnostics
      ReadOnly Property TypeName As String
      ReadOnly Property MethodName As String

      ReadOnly Property ArgsType() As String()
      ReadOnly Property IndexOfFormat() As Integer

      Function Validate( fs As String, args As Object() ) As IEnumerable(Of IReportIssue)

    End Interface

  End Namespace

End Namespace
