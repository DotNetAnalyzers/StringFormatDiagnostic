<System.ComponentModel.ImmutableObject(True)>
Public Class ParsedChar
  Public ReadOnly Property Index As Integer
  Property _Source As ISourceText
  Dim _NC As ParsedChar = Nothing
  Dim _NS As Boolean = False
  Dim _BC As ParsedChar = Nothing
  Dim _BS As Boolean = False

  Public Sub New(Source As ISourceText, Index As Integer)
    _Index = Index : _Source = Source
  End Sub

  Public Function IsEoT() As Boolean
    Return _Index > _Source.EndIndex 
  End Function
  Public Function IsNotEoT() As Boolean
    Return _Index <= _Source.EndIndex  
  End Function
  Public Function IsBoT() As Boolean
    Return _Index < 0
  End Function
  Public Function IsNotBoT() As Boolean
    Return _Index >= 0
  End Function
  Public Function [Next]() As ParsedChar
    ' If already set then return cached verison
    If _NS Then Return _NC

    _NC = If(Index < _Source.EndIndex, New ParsedChar(_Source, Index + 1), Nothing)
    _NS = True
    Return _NC
  End Function

  Public Function Back() As ParsedChar
    ' If already set then return cached verison
    If _BS Then Return _BC
    _BC = If(Index > 0, New ParsedChar(_Source, Index - 1), Nothing)
    _BS = True
    Return _BC
  End Function

  Public ReadOnly Property Value As Char
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
