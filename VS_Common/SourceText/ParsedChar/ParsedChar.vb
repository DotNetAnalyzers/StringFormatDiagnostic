Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces

<System.ComponentModel.ImmutableObject(True)>
Public Class ParsedChar
  Implements IParsedChar

  Public ReadOnly Property Index As Integer Implements IParsedChar.Index 
  Public ReadOnly Property Source As ISourceText Implements IParsedChar.SourceText 
  Dim _NC As ParsedChar = Nothing
  Dim _NS As Boolean = False
  Dim _BC As ParsedChar = Nothing
  Dim _BS As Boolean = False

  Public Sub New(Source As ISourceText, Index As Integer)
    _Index = Index : Me._Source = Source
  End Sub

  Public Function [Next]() As IParsedChar  Implements IParsedChar.[Next]
    ' If already set then return cached verison
    If _NS Then Return _NC

    _NC = If(Index < _Source.EndIndex, New ParsedChar(_Source, Index + 1), Nothing) ' New ParsedChar(_Source,Index))
    _NS = True
    Return _NC
  End Function

  Public Function Back() As IParsedChar Implements IParsedChar.Back
    ' If already set then return cached verison
    If _BS Then Return _BC
    _BC = If(Index >= 0, New ParsedChar(_Source, Index - 1), New ParsedChar(_Source, -1))
    _BS = True
    Return _BC
  End Function

  Public ReadOnly Property Value As Char  Implements IParsedChar.Value  
    Get
      Return _Source(Index)
    End Get
  End Property

  Public Overrides Function ToString() As String
    Return String.Format("({0}):=[{1}]", Index, Value)
  End Function

  Shared Operator <>(pc As ParsedChar, c As Char) As Boolean
    Return pc.Value <> c
  End Operator

  Shared Operator =(pc As ParsedChar, c As Char) As Boolean
    Return pc.Value = c
  End Operator

  Shared Operator <(pc As ParsedChar, c As Char) As Boolean
    Return pc.Value < c
  End Operator

  Shared Operator <=(pc As ParsedChar, c As Char) As Boolean
    Return pc.Value <= c
  End Operator

  Shared Operator >(pc As ParsedChar, c As Char) As Boolean
    Return pc.Value > c
  End Operator

  Shared Operator >=(pc As ParsedChar, c As Char) As Boolean
    Return pc.Value >= c
  End Operator

End Class
