Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces
Imports AdamSpeight2008.StringFormatDiagnostic

#Const _Define_Alphabetic_ = 1

Namespace Global.AdamSpeight2008.StringFormatDiagnostic
  <HideModuleName>
  Public Module CommonExts
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

  End Module

  <HideModuleName()>
  Public Module ParsedChar_Exts
    <Extension>
    Public Function IsWhitespace(cp As IParsedChar) As Boolean
      Return (cp IsNot Nothing) AndAlso Char.IsWhiteSpace(cp.Value)
    End Function

#If _Define_Alphabetic_ = 0 Then
    <Extension()>
    Public Function IsLetter(pc As ParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso Char.IsLetter(pc.Value)
    End Function
#Else
    <Extension()>
    Public Function IsLetter(pc As IParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso ((pc.Value >= "A"c AndAlso pc.Value <= "Z"c) OrElse (pc.Value >= "a"c AndAlso pc.Value <= "z"c))
    End Function
#End If
    <Extension>
    Public Function IsEoT(pc As IParsedChar) As Boolean
      Return pc Is Nothing
    End Function

    <Extension>
    Public Function IsNotEoT(pc As IParsedChar) As Boolean
      Return pc IsNot Nothing
    End Function

    Public Function ParseDigits(pc As IParsedChar) As OutputResult(Of String)
      Dim curr = pc
      Dim res As New OutputResult(Of String) With {.Output = ""}
      While curr.IsNotEoT AndAlso curr.IsDigit
        res.Output &= curr.Value
        curr = curr.Next
      End While
      Return res.LastParse(curr)
    End Function

    <Extension>
    Public Function RepCount(pc As IParsedChar, c As Char) As OutputResult(Of Integer)
      Dim res As New OutputResult(Of Integer)
      Dim curr = pc
      While curr.IsNotEoT AndAlso (curr.Value = c)
        res.Output += 1
      End While
      Return res.LastParse(curr)
    End Function

    <Extension>
    Public Function IsDigit(c As Char) As Boolean
      Return Char.IsDigit(c)
    End Function

    <Extension>
    Public Function IsDigit(pc As IParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso ((pc.Value >= "0"c) AndAlso (pc.Value <= "9"c))
    End Function

    <Extension>
    Public Function IsLetterOrWhitespace(pc As IParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso (pc.IsLetter OrElse pc.IsWhitespace)
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

  End Module
End Namespace
