Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft
Imports Common

Namespace Global.Roslyn.StringFormatDiagnostics
  Public Module ParsedChar_Exts
    <Extension>
    Public Function IsWhitespace(cp As ParsedChar) As Boolean
      Return (cp IsNot Nothing) AndAlso Char.IsWhiteSpace(cp.Value)
    End Function

    <Extension()>
    Public Function IsLetter(pc As ParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso Char.IsLetter(pc.Value)
    End Function

    <Extension>
    Public Function IsLetterOrWhitespace(pc As ParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso (pc.IsLetter OrElse pc.IsWhitespace)
    End Function

    <Extension>
    Public Function ContainsMoreThan(fs As String, NoMoreThan As Integer, pred As Func(Of Char, Boolean)) As Boolean
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
