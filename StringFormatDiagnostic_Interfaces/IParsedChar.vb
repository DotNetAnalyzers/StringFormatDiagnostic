Namespace Global.AdamSpeight2008.StringFormatDiagnostic.Interfaces

  Public Interface IParsedChar
    Function [Next]() As IParsedChar
    Function Back() As IParsedChar
    ReadOnly Property Value As Char
    ReadOnly Property SourceText As ISourceText
    ReadOnly Property Index As Integer 


  End Interface

End Namespace
