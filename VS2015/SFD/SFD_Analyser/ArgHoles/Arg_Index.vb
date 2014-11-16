'Option Strict On

Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Common

  Public Class Arg_Index
    Inherits Arg_Identifier

    Public Sub New(Span As IndexSpan?, ArgIndex As Integer)
      MyBase.New(Span)
      _ArgIndex = ArgIndex
    End Sub

    Public ReadOnly Property ArgIndex As Integer
    Public Overrides Function ToString() As String
      Return String.Format("{0}{1}", ArgIndex, MyBase.ToString)
    End Function
  End Class

End Namespace
