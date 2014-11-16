'Option Strict On
Imports System.Linq.ImmutableArrayExtensions
Imports System.Threading
Imports Microsoft.CodeAnalysis

Imports AdamSpeight2008.StringFormatDiagnostics.Results
Imports AdamSpeight2008.StringFormatDiagnostics.Errors

Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Common
  Public Module Common_StringFormat

    Public Function AnalyseFormatString(ct As CancellationToken,
                                            format As String,
                                            IndexOffset As Integer,
                                            Provider As IFormatProvider,
                                            Args As IEnumerable(Of Object)) As Base_Result
      Dim res As New Result(Of String)("")
      Dim NumOfArgs = Args.Count
      Dim unused As New HashSet(Of Integer)(Enumerable.Range(0, NumOfArgs))
      Dim holes = SFD.Yield_ArgHoles(format, IndexOffset).ToArray
      If (NumOfArgs = 0) AndAlso (holes.Count = 0) Then Return res
      If (NumOfArgs = 0) AndAlso (holes.Count > 0) Then res.AddWarning(New Warnings.NoArgs) : Return res
      If (NumOfArgs > 0) AndAlso (holes.Count = 0) Then res.AddWarning(New Warnings.NoHoles) : Return res
      For Each hole In holes
        If TypeOf hole Is Error_Result Then res.AddError(DirectCast(hole, Error_Result).Error) : Continue For
        If TypeOf hole IsNot Result(Of ArgHole) Then Continue For
        Dim AHole = DirectCast(hole, Result(Of ArgHole)).Value
        If AHole.Identifier.Exists = False Then
          res.AddError(New Errors.MissingArgIndex(AHole.Span))
          If AHole.Alignment.Exists AndAlso (Math.Abs(AHole.Alignment.Alignment) > _Limit_) Then res.AddError(New Errors.AligmentValueExceeded(AHole.Alignment.Span, AHole.Alignment.Alignment, _Limit_))
        Else



          If TypeOf AHole.Identifier IsNot Arg_Index Then Continue For
          Dim ai = DirectCast(AHole.Identifier, Arg_Index)
          If ai.ArgIndex < 0 Then res.AddError(New Errors.NegativeArgIndex(ai.Span)) : Continue For
          If ai.ArgIndex >= _Limit_ Then res.AddError(New Errors.ArgIndexBeyondLimit(ai.Span, ai.ArgIndex, _Limit_))
          If ai.ArgIndex >= NumOfArgs Then res.AddError(New Errors.ArgIndexBeyondEnd(ai.Span, ai.ArgIndex, NumOfArgs)) : Continue For
          ' Arg.Index is within bounds
          unused.Remove(ai.ArgIndex)
          If AHole.Alignment.Exists AndAlso (Math.Abs(AHole.Alignment.Alignment) > _Limit_) Then res.AddError(New Errors.AligmentValueExceeded(AHole.Alignment.Span, AHole.Alignment.Alignment, _Limit_))
          If AHole.Format.Exists = False Then Continue For
          ' validate formatting
          Dim arg = Args(ai.ArgIndex)
          Dim ahf = DirectCast(AHole.Format, Arg_Format)
          Dim result = Valid_Format_For_Type(ct, ahf.Format, arg, ahf.Span.Value.Index)
          result.Errors.ToList.ForEach(Sub(e) res.AddError(e))
          result.Warnings.ToList.ForEach(Sub(w) res.AddWarning(w))
        End If
      Next
      For Each u In unused
        res.AddWarning(New Warnings.UnusedArg(u))
      Next
      Return res
    End Function

    Private Const _Limit_ = 1000000

    Private Function Valid_Format_For_Type(ct As CancellationToken, fs As String, obj As Object, i As Integer) As Base_Result
      Dim Result As New Result(Of String)("")
      Dim objTN = obj.GetType.ToString
      Dim toStringMehods = From a In Analysers.ToStringAnalysers
                           Where a.Key = objTN
                           Select a.Value
      Return If(toStringMehods.Any, toStringMehods(0)(ct, fs, i, Nothing, {}), Result)
    End Function

  End Module

End Namespace
