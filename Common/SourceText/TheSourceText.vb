Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces

Public Class TheSourceText
  Implements ISourceText

  Private _Source As String
  Private _EndIndex As Integer

  Public Sub New(Source As String)
    _Source = Source
    _EndIndex = _Source.Length - 1
  End Sub

  Default Public ReadOnly Property Chars(Index As Integer) As Char Implements ISourceText.Chars
    Get
      Return If((0 <= Index) AndAlso (Index <= EndIndex), _Source(Index), Nothing)
    End Get
  End Property

  Public ReadOnly Property EndIndex As Integer Implements ISourceText.EndIndex
    Get
      Return _EndIndex
    End Get
  End Property
End Class