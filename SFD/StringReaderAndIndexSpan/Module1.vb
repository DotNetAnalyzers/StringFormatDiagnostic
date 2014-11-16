Imports System.Runtime.CompilerServices

Namespace Global.AdamSpeight2008

  Public Class StringReader
    Private _s As String
    Public ReadOnly Property Index As Integer
    Private _marked_ = False
    Private _MarkIndex_ = 0

    Public Sub New(s As String)
      _s = s
      _Index = 0
    End Sub
    Private Sub New(s As String, index As Integer)
      Me.New(s)
      _Index = index
    End Sub

    Public Function IsNotEoT() As Boolean
      Return Index < _s.Length
    End Function

    Public Function IsEoT() As Boolean
      Return Not (IsNotEoT())
    End Function

    Public Function Value() As Char?
      Return If(IsNotEoT(), New Char?(_s(Index)), New Char?())
    End Function

    Public Function Peek() As Char?
      Return If(Index + 1 < _s.Length, New Char?(_s(Index + 1)), New Char?())
    End Function

    Public Sub [Next]()
      If IsNotEoT() Then _Index += 1
    End Sub

    Public Shared Operator =(x As StringReader, c As Char) As Boolean
      Dim xr = x.Value() : Return If(xr.HasValue = False, False, xr.Value = c)
    End Operator

    Public Shared Operator <>(x As StringReader, c As Char) As Boolean
      Dim xr = x.Value() : Return If(xr.HasValue = False, False, xr.Value = c)
    End Operator

    Public Function Mark() As Boolean
      If _marked_ Then Return False
      _MarkIndex_ = Me.Index
      _marked_ = True
      Return True
    End Function

    Public Function Unmark() As IndexSpan?
      If _marked_ = False Then Return New IndexSpan?()
      Dim result As New IndexSpan(_MarkIndex_, Me.Index)
      _marked_ = False
      Return result
    End Function

    Public Overrides Function ToString() As String
      Return String.Format("({0})=[{1}]", Me.Index, If(IsNotEoT(), Value(), ""))
    End Function


    Public Function Copy() As StringReader
      Return New StringReader(Me._s, Me.Index)
    End Function
  End Class

  Public Structure IndexSpan
    Public ReadOnly Index As Integer
    Public ReadOnly Span As Integer

    Public Sub New(Index As Integer, Span As Integer)
      Me.Index = Index
      Me.Span = Span
    End Sub

    Public Overrides Function ToString() As String
      Return String.Format("[{0,-4}::{1,4}]", Index, Span)
    End Function

    Public Shared Operator +(s As IndexSpan, o As Integer) As IndexSpan
      Return New IndexSpan(s.Index + o, s.Span)
    End Operator
  End Structure


End Namespace