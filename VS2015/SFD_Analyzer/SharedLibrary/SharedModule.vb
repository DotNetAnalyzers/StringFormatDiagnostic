Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
Imports SFD.Analysis

Namespace Global.SFD
  Public Module Common
    Public Const _QUOTE_ As Char = """"c

    <Runtime.CompilerServices.Extension>
    Public Function DeQuoted(s As String) As String
      ' If a string is included in double qoutes (") remove the match pair.
      If s Is Nothing Then Return ""
      If (s.Length > 1) AndAlso s.EndsWith(_QUOTE_) AndAlso s.StartsWith(_QUOTE_) Then s = s.Substring(1, s.Length - 2)
      Return s
    End Function

    Function CreateDiagnostic(description As DiagnosticDescriptor,
                        ByRef context As SyntaxNodeAnalysisContext,
                        ts As TextSpan,
                        sk As SFD.StringFormat.Span,
                        msg As String) As Diagnostic
      Return Diagnostic.Create(description, Location.Create(context.SemanticModel.SyntaxTree,
                          TextSpan.FromBounds(ts.Start + sk.Start + 1, ts.Start + sk.Finish + 1)), msg)
    End Function
    Public Sub CreateDiagnosticReport(description As DiagnosticDescriptor,
                           ByRef context As SyntaxNodeAnalysisContext,
                           ts As TextSpan,
                           sk As SFD.StringFormat.Span,
                           msg As String)
      context.ReportDiagnostic(CreateDiagnostic(description, context, ts, sk, msg))
    End Sub



    Public Sub AddError(I As DiagnosticInfo, id As String, msg As String, ByRef context As SyntaxNodeAnalysisContext, fs As TextSpan, sk As StringFormat.Span,
                 Optional atSource As Boolean = False)
      Dim r As New DiagnosticDescriptor(id, "", msg, I.Cat, DiagnosticSeverity.Error, True)
      CreateDiagnosticReport(r, context, fs, If(atSource, New SFD.StringFormat.Span(sk.Source, 0, fs.Length), sk), msg)
    End Sub

    Public Sub AddWarning(I As DiagnosticInfo, id As String, msg As String, context As SyntaxNodeAnalysisContext, fs As TextSpan, sk As StringFormat.Span,
                   Optional atSource As Boolean = False)
      Dim r As New DiagnosticDescriptor(id, "", msg, I.Cat, DiagnosticSeverity.Warning, True)
      CreateDiagnosticReport(r, context, fs, If(atSource, New StringFormat.Span(sk.Source, 0, fs.Length), sk), msg)
    End Sub

    Public Sub AddHidden(I As DiagnosticInfo, id As String, msg As String, ByRef context As SyntaxNodeAnalysisContext, fs As TextSpan, sk As StringFormat.Span,
                  Optional atSource As Boolean = False)
      Dim r As New DiagnosticDescriptor(id, "", msg, I.Cat, DiagnosticSeverity.Hidden, True)
      CreateDiagnosticReport(r, context, fs, If(atSource, New StringFormat.Span(sk.Source, 0, fs.Length), sk), msg)
    End Sub

    Public Sub AddInfo(I As DiagnosticInfo, id As String, msg As String, ByRef context As SyntaxNodeAnalysisContext, fs As TextSpan, sk As StringFormat.Span,
                 Optional atSource As Boolean = False)
      Dim r As New DiagnosticDescriptor(id, "", msg, I.Cat, DiagnosticSeverity.Info, True)
      CreateDiagnosticReport(r, context, fs, If(atSource, New StringFormat.Span(sk.Source, 0, fs.Length), sk), msg)
    End Sub

  End Module
End Namespace

