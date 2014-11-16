'Option Strict On
Imports System.Linq.ImmutableArrayExtensions
Imports System.Threading
Imports Microsoft.CodeAnalysis

Imports AdamSpeight2008.StringFormatDiagnostics.Results
Imports AdamSpeight2008.StringFormatDiagnostics.Errors
Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Common

  Public MustInherit Class Arg_Identifier
    Inherits Arg_Base

    Friend Sub New(Span As IndexSpan?)
      MyBase.New(Span)
    End Sub
    Public Overrides Function ToString() As String
      Return MyBase.ToString()
    End Function

  End Class

End Namespace