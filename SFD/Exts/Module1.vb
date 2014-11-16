Imports System.Runtime.CompilerServices

Namespace Global.AdamSpeight2008
  <HideModuleName>
  Public Module CommonExts

    <Extension>
    Public Function Exists(Of t As Class)(c As t) As Boolean
      Return c IsNot Nothing
    End Function

    <Extension>
    Public Function AnyIsNull(Of T As Class)(a() As T) As Boolean
      If a Is Nothing Then Return True
      For i = 0 To a.Count - 1
        If a(i) Is Nothing Then Return True
      Next
      Return False
    End Function

    <Extension>
    Public Function IfAnyThenDo(Of T)(xs As IEnumerable(Of T), act As Action(Of IEnumerable(Of T))) As IEnumerable(Of T)
      If xs.Any Then act(xs)
      Return xs
    End Function

    <Extension>
    Public Sub Add(Of K, V)(cd As Concurrent.ConcurrentDictionary(Of K, V), key As K, value As V)
      cd.AddOrUpdate(key, Function(kv) value, Function(kv, va) value)
    End Sub

    <Extension>
    Public Function Are(Of T As IComparable(Of T))(xs As IEnumerable(Of T), ys As IEnumerable(Of T)) As Boolean
      If {xs, ys}.AnyIsNull Then Return False
      Return xs.SequenceEqual(ys)
    End Function

    <Extension>
    Public Function BeginsWith(Of T As IComparable(Of T))(xs As IList(Of T), ys As IList(Of T)) As Boolean
      If {xs, ys}.AnyIsNull Then Return False
      If xs.Count < ys.Count Then Return False
      For i = 0 To ys.Count - 1
        If xs(i).CompareTo(ys(i)) <> 0 Then Return False
      Next
      Return True
    End Function

    Public Enum lusive As Integer
      Inc = 1
      Exc = 0
    End Enum




    <Extension>
    Function IsBetween(Of T As IComparable(Of T))(value As T,
                                                   lowerValue As T,
                                                   upperValue As T,
                                                   Optional lclus As lusive = lusive.Inc,
                                                   Optional uclus As lusive = lusive.Exc
                                                   ) As Boolean
      Return (lowerValue.CompareTo(value) <= lclus) AndAlso (value.CompareTo(upperValue) <= uclus)
    End Function


    <Extension>
    Public Function ContainsMoreThan(fs As String, NoMoreThan As Integer, pred As Func(Of Char, Boolean)) As Boolean
      If AnyIsNull(Of Object)({fs, pred}) Then Return False
      Dim count, index As Integer
      While index < fs.Count
        If pred(fs(index)) Then
          count += 1
          If count > NoMoreThan Then Return True
        End If
        index += 1
      End While
      Return False
    End Function


#Region "Coomonly used Characters"
    Public Const Opening_Brace As Char = "{"c
    Public Const Closing_Brace As Char = "}"c
    Public Const _SPACE_ As Char = " "c
    Public Const _COMMA_ As Char = ","c
    Public Const _COLON_ As Char = ":"c
    Public Const _MINUS_ As Char = "-"c
    Public Const _QUOTE_ As Char = """"c
#End Region
    <Extension>
    Public Function DeQuoted(s As String) As String
      ' If a string is included in double qoutes (") remove the match pair.
      If s Is Nothing Then Return ""
      If (s.Length > 0) AndAlso (s.Last = _QUOTE_) Then s = s.Substring(0, s.Length - 1)
      If (s.Length > 0) AndAlso (s.First = _QUOTE_) Then s = s.Substring(1)
      Return s
    End Function
  End Module
End Namespace