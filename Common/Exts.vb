Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft
Imports Common
Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces
Imports AdamSpeight2008.StringFormatDiagnostic

#Const _Define_Alphabetic_ = 1

Namespace Global.AdamSpeight2008.StringFormatDiagnostic
  Public Module CommonExts
    <Extension>
    Public Function Are(Of T As IComparable(Of T))(xs As IEnumerable(Of T), ys As IEnumerable(Of T)) As Boolean
      If xs Is Nothing Then Return False
      If ys Is Nothing Then Return False 
      Return xs.SequenceEqual(ys)
    End Function
    <Extension>
    Public Function Begins(Of T As IComparable(Of T))(xs As IList(Of T), ys As IList(Of T)) As Boolean
      If xs Is Nothing Then Return False 
      If ys Is Nothing Then Return False 
      If xs.Count < ys.Count Then Return False
      For i = 0 To ys.Count - 1
        If xs(i).CompareTo(ys(i)) <> 0 Then Return False
      Next
      Return True
    End Function
  End Module

  <HideModuleName()>
  Public Module ParsedChar_Exts
    <Extension>
    Public Function IsWhitespace(cp As IParsedChar) As Boolean
      Return (cp IsNot Nothing) AndAlso Char.IsWhiteSpace(cp.Value)
    End Function

#If _Define_Alphabetic_ = 0
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
      res.LastParse = curr
      Return res
    End Function

    <Extension>
    Public Function RepCount(pc As IParsedChar, c As Char) As OutputResult(Of Integer)
      Dim res As New OutputResult(Of Integer)
      Dim curr = pc
      While curr.IsNotEoT AndAlso (curr.Value = c)
        res.Output += 1
      End While
      res.LastParse = curr
      Return res
    End Function

    <Extension>
    Public Function IsDigit(c As Char) As Boolean
      Return Char.IsDigit(c)
    End Function

    <Extension>
    Public Function IsDigit(pc As IParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso  ((pc.Value >= "0"c) AndAlso (pc.Value <= "9"c))
    End Function

    <Extension>
    Public Function IsLetterOrWhitespace(pc As IParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso (pc.IsLetter OrElse pc.IsWhitespace)
    End Function

    <Extension>
    Public Function ContainsMoreThan(fs As String, NoMoreThan As Integer, pred As Func(Of Char, Boolean)) As Boolean
      If fs Is Nothing Then Return False
      If pred Is Nothing Then Return False 
      Dim count = 0
      Dim index = 0
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
