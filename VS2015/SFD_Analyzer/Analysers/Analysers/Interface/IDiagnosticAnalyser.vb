Imports System.Threading
Imports Analysers

Namespace Global.SFD.Analysis

  Public Interface IDiagnosticAnalyser
    Function Analyse(ct As CancellationToken,
                     span As SFD.StringFormat.SpanKind,
                    text As String,
                   FormatIndex As Integer,
                   provider As IFormatProvider,
                   Args As IEnumerable(Of Object)) As IEnumerable(Of Issues.Base_Issue)
  End Interface

End Namespace
