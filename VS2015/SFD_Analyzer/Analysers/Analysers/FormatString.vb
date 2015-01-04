Imports System.Threading
Imports Analysers
Imports SFD.StringFormat
Imports SFD.StringFormat.StringFormat

Namespace Global.SFD.Analysis

  Public Class FormatString
    Implements IDiagnosticAnalyser


    Public Function Analyse(ct As CancellationToken,
                           span As SpanKind,
                           text As String,
                    FormatIndex As Integer,
                       provider As IFormatProvider,
                           Args As IEnumerable(Of Object)
                              ) As IEnumerable(Of Base_Issue) Implements IDiagnosticAnalyser.Analyse
      Dim Issues As IEnumerable(Of Issues.Base_Issue)
      Issues = Analyse_StringFormat(ct, span, Args.ToArray)
      Return Issues
    End Function

    Function Analyse_Args_Hole_Counts(ByRef Issues As List(Of Base_Issue), om As SpanKind, HoleCount As Integer, ArgsCount As Integer) As Boolean
      If (ArgsCount = 0) AndAlso (HoleCount = 0) Then Return False
      If (ArgsCount = 0) AndAlso (HoleCount > 0) Then Issues.Add(New Issues.NoArgs(om)) : Return True 
      If (HoleCount = 0) AndAlso (ArgsCount > 0) Then Issues.Add(New Issues.NoHoles(om)) : Return True 
      Return False 
    End Function

    Function Analyse_StringFormat(ct As CancellationToken, om As SpanKind, args As Object()) As IEnumerable(Of Issues.Base_Issue)
      Dim Issues As New List(Of Issues.Base_Issue)
      Try
        If om = Kinds.FormatString Then
          Dim HoleCount = om.MadeOf.Where(Function(st) st.Kind = Kinds.ArgHole).Count
          Dim ArgsCount = args.Count
          Dim _ERRS_ = om.MadeOf.Any(Function(st) st.HasError)
          'If _ERRS_ = False Then
            If Analyse_Args_Hole_Counts(Issues, om, HoleCount, ArgsCount) Then Return Issues
          'End If

          Dim unused As New HashSet(Of Integer)(Enumerable.Range(0, ArgsCount))

          For Each part In om.MadeOf
            Select Case part.Kind
              Case Kinds.ArgHole               : Issues.AddRange( Analyse_ArgHole(ct, part, HoleCount, args, unused))
              Case Kinds.Err_Malformed_ArgHole : Issues.AddRange( Analyse_MalformedArgHole(ct, part, HoleCount, args, unused))
              Case Kinds.Err_UC                : Issues.Add(New Issues.Err_UC(part,part.GetText(0)))
            End Select
          Next
          ' ToDo: Span (om) needs changing to the span of the Argument referred to.
          If unused.Count > 0 Then Issues.AddRange(unused.Select(Of Issues.Base_Issue)(Function(n) New Issues.Unused_Arg(om, n)))
        End If
      Catch ex As Exception
        Debug.WriteLine(ex.ToString)
      End Try

      Return Issues
    End Function

    Private Const _DotNetLimit_ As Integer = 1000000
   Function Analyse_MalformedArgHole(ct As CancellationToken,
                                      argHole As SpanKind,
                                      HoleCount As Integer,
                                      args As Object(),
                                      unused As HashSet(Of Integer)
                                      ) As IEnumerable(Of Issues.Base_Issue)
      Dim Issues As New List(Of Issues.Base_Issue)
      If argHole(0) = Kinds.Err_Malformed_ArgHole Then Return Analyse_MalformedArgHole(ct,argHole(0),HoleCount,args,unused)
      Dim LB = argHole.MadeOf.FirstOrDefault(Function(sk) sk.Kind = Kinds.BL)
      If LB Is Nothing Then Issues.Add(New Missing_LB(argHole.Offset(0, 1, Kinds.Missing_LB))) : Return Issues
      Dim RB = argHole.MadeOf.FirstOrDefault(Function(sk) sk.Kind = Kinds.BR)
      If RB Is Nothing Then Issues.Add(New Missing_RB(SpanKind.MakeFrom(Kinds.Missing_RB,arghole.GetSourceText,argHole.Finish - 1, argHole.Finish))) : Return Issues
      Try
        Dim ArgCount = args.Count
        Dim parts = argHole.MadeOf
        If argHole.Kind = Kinds.Err_Malformed_ArgHole Then
          Dim ArgIndex = parts.FirstOrDefault(Function(st) st.Kind = Kinds.Arg_Index )
          If ArgIndex Is Nothing Then
            ' This span needs altering        
            Issues.Add(New Issues.Err_NoArgIndex(argHole))
          Else
            ArgIndex_Analysis(Issues, unused, ArgIndex, ArgCount)

          End If
          Dim ArgAlign = parts.FirstOrDefault(Function(st) st.Kind = Kinds.Arg_Align Or st.Kind = Kinds.Err_Malformed_ArgAlign)
          If ArgAlign IsNot Nothing Then
            If ArgAlign.Kind = Kinds.Arg_Align Then
            ArgAlign_Analysis(Issues, unused, ArgAlign, ArgCount)
            Else If  ArgAlign.Kind = Kinds.Err_Malformed_ArgAlign Then
              'Issues.Add(New Issues.Err_NoAlign(ArgAlign))

            End If
          End If


          Dim ArgFormat = parts.FirstOrDefault(Function(st) st.Kind = Kinds.Arg_Format)
          If ArgFormat IsNot Nothing Then
            'Dim ahf = ArgFormat.GetText
            'Dim arg = args(ArgIndex_Value)
            'Dim result = Valid_Format_For_Type(ct, ArgFormat, ahf, arg, ArgIndex_Value)
            'Issues.AddRange(result)

          End If
          For Each A In parts.Where(Function(st) st.HasError)
            Select Case A.Kind
              Case Kinds.Err_Malformed_ArgAlign : Issues.Add(New Issues.Err_NoAlign(A))
              Case Kinds.Err_UC : Issues.Add(New Issues.Err_UC(A,A.GetText(0)))

            End Select
          Next
        End If
      Catch ex As Exception
        Debug.WriteLine(ex.ToString)
      End Try
      Return Issues
    End Function

 Function ArgIndex_Analysis(ByRef issues As List(Of Base_Issue),
                                ByRef unused As HashSet(Of Integer),
                                ArgIndex As SpanKind,
                                ArgCount As Integer) as Integer
      Dim ArgIndex_Value = 0
      If Integer.TryParse(ArgIndex.GetText, ArgIndex_Value) Then
        If ArgIndex_Value < 0              Then issues.Add(New Issues.ArgIndex_Negative(ArgIndex)) : Return ArgIndex_Value
        If ArgIndex_Value >= _DotNetLimit_ Then issues.Add(New Issues.ArgIndex_DotNetLimit_Exceeded(ArgIndex)) : Return ArgIndex_Value
        If ArgIndex_Value >= ArgCount      Then issues.Add(New Issues.ArgIndex_Exceeded(ArgIndex_Value, ArgCount, ArgIndex)) : Return ArgIndex_Value
        unused.Remove(ArgIndex_Value)
     Else
        issues.Add(New Issues.NotAnInteger(ArgIndex))
      End If
      Return ArgIndex_Value 
    End Function 

    Sub ArgAlign_Analysis(ByRef issues As List(Of Base_Issue),
                                ByRef unused As HashSet(Of Integer),
                                ArgAlign As SpanKind,
                                ArgCount As Integer)

      Dim ArgAlign_Value = 0
      If Integer.TryParse(ArgAlign.GetText, ArgAlign_Value) Then
        If ArgAlign_Value >=  _DotNetLimit_ Then issues.Add(New Issues.ArgAlign_DotNetLimit_Exceeded(ArgAlign)) : Return
        If ArgAlign_Value <= -_DotNetLimit_ Then issues.Add(New Issues.ArgAlign_DotNetLimit_Exceeded(ArgAlign)) : Return
        unused.Remove(ArgAlign_Value)
      Else
        issues.Add(New Issues.NotAnInteger(ArgAlign))
      End If
    End Sub

 
    Function Analyse_ArgHole(ct As CancellationToken, arghole As SpanKind, HoleCount As Integer, args As Object(), unused As HashSet(Of Integer)) As IEnumerable(Of Issues.Base_Issue)
      Dim Issues As New List(Of Issues.Base_Issue)
      Dim ArgCount = args.Count
      Dim parts = arghole.MadeOf

      Dim ArgIndex = parts.FirstOrDefault(Function(st) st.Kind = Kinds.Arg_Index)
      Dim ArgIndex_Value = ArgIndex_Analysis(Issues, unused, ArgIndex, ArgCount)

      Dim ArgAlign = parts.FirstOrDefault(Function(st) st.Kind = Kinds.Arg_Align)
      If ArgAlign IsNot Nothing Then ArgAlign_Analysis(Issues,unused,ArgAlign,ArgCount)

      Dim ArgFormat = parts.FirstOrDefault(Function(st) st.Kind = Kinds.Arg_Format)
      If ArgFormat IsNot Nothing Then
        Dim ahf = ArgFormat.GetText
        Dim arg = args(ArgIndex_Value)
        Dim result = Valid_Format_For_Type(ct, ArgFormat, ahf, arg, ArgIndex_Value)
        Issues.AddRange(result)
      End If

      Return Issues
    End Function

    Private Function Valid_Format_For_Type(ct As CancellationToken, ArgFormat As SpanKind, fs As String, obj As Object, i As Integer) As IEnumerable(Of Issues.Base_Issue)
      Dim Result As New List(Of Issues.Base_Issue)
      Dim objTN = obj.GetType.ToString
      Dim toStringMehods = From a In Analysers.ToStringAnalysers
                           Where a.Key = objTN
                           Select a.Value
      Return If(toStringMehods.Any, toStringMehods(0).Analyse(ct, ArgFormat, fs, i, Nothing, {}), Result)
    End Function


  End Class
End Namespace

