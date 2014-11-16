'Option Strict On

Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Common

  Public Class Arg_Alignment
    Inherits Arg_Base
    Public ReadOnly Property Alignment As Integer

    Public Sub New(Span As IndexSpan?, Alignment As Integer)
      MyBase.New(Span)
      _Alignment = Alignment
    End Sub
    Public Overrides Function ToString() As String
      Return String.Format("{0}{1}", Alignment, MyBase.ToString)
    End Function
  End Class

End Namespace
