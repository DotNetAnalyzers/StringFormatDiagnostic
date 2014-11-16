'Option Strict On
Imports System.Linq.ImmutableArrayExtensions
Imports System.Threading
Imports Microsoft.CodeAnalysis

Imports AdamSpeight2008.StringFormatDiagnostics.Results
Imports AdamSpeight2008.StringFormatDiagnostics.Errors
Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Common

  Public MustInherit Class Arg_Base
    Public ReadOnly Property Span As IndexSpan?

    Friend Sub New(Span As IndexSpan?)
      _Span = Span
    End Sub
    Public Overrides Function ToString() As String
      Return Span.ToString
    End Function
  End Class

End Namespace
