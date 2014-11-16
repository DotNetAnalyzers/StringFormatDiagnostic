'Option Strict On

Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Common

  Public Class ArgHole
    Public ReadOnly Property Identifier As Arg_Identifier
    Public ReadOnly Property Alignment As Arg_Alignment
    Public ReadOnly Property Format As Arg_Format
    Public ReadOnly Property Span As IndexSpan

    Public Sub New(Span As IndexSpan, Identifier As Arg_Identifier, Alignment As Arg_Alignment, Format As Arg_Format)
      Me._Span = Span
      Me._Identifier = Identifier
      Me._Alignment = Alignment
      Me._Format = Format
    End Sub

    Public Overrides Function ToString() As String
      Return String.Format("({0} , {1} : {2})", If(Identifier, ""), If(Alignment, ""), If(Format, ""))
    End Function
  End Class

End Namespace