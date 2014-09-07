Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft
Imports Common

#Const _Define_Alphabetic_ = 1

Namespace Global.Roslyn.StringFormatDiagnostics
  Public Module ParsedChar_Exts
    <Extension>
    Public Function IsWhitespace(cp As ParsedChar) As Boolean
      Return (cp IsNot Nothing) AndAlso Char.IsWhiteSpace(cp.Value)
    End Function

    #If _Define_Alphabetic_ = 0 
    <Extension()>
    Public Function IsLetter(pc As ParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso Char.IsLetter(pc.Value)
    End Function
    #Else
    <Extension()>
    Public Function IsLetter(pc As ParsedChar) As Boolean
      Return (pc IsNot Nothing) AndAlso ((pc.Value>="A"c AndAlso pc.Value<="Z"c) OrElse (pc.Value>="a"c AndAlso pc.Value<="z"c))
    End Function
#End If
    <Extension>
    Public Function IsEoT(pc As ParsedChar) As Boolean
      Return pc Is Nothing
    End Function

    <Extension>
    Public Function IsNotEoT(pc As ParsedChar) As Boolean
      Return pc IsNot Nothing
    End Function

    Public Function ParseDigits(pc As ParsedChar) As OutputResult(Of String)
      Dim curr = pc
      Dim res As New OutputResult(Of string)With {.Output =""}
      While curr.IsNotEoT AndAlso Curr.IsDigit
        res.Output &= curr.Value
        curr =curr.next        
      End While
      res.LastParse = curr
      Return res
    End Function

    <Extension>
    Public Function RepCount(pc As ParsedChar, c As Char) As OutputResult(Of Integer)
      Dim res As New OutputResult(Of Integer)
      Dim curr = pc
      While curr.IsNotEoT AndAlso (curr = c)
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
    Public Function IsDigit(pc As ParsedChar) As Boolean
      Return (pc >= "0"c) AndAlso (pc <="9"c)
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
