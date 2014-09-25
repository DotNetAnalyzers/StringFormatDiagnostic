Option Strict On
Imports System.Threading

Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax


Namespace Global.AdamSpeight2008.StringFormatDiagnostic.Common
  <HideModuleName>
  Public Module Common

    Public Enum Lang As Integer
      VB = 0
      CS = 1
    End Enum

    Private _LangAnalysers As New Dictionary(Of Lang, Concurrent.ConcurrentDictionary(Of String, Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object)))
)
    Public Sub AddLanguageAnalysers(l As Lang, cd As Concurrent.ConcurrentDictionary(Of String, Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object))))
      If _LangAnalysers.ContainsKey(l) Then Exit Sub
      _LangAnalysers.Add(l, cd)
    End Sub

    Private Function GetLangAnalyser(l As Lang, a As String) As Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object))
      Dim act As Action(Of MemberAccessExpressionSyntax, Microsoft.CodeAnalysis.SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object)) = Nothing
      Dim r = _LangAnalysers(l).TryGetValue(a, act)
      Return If(r, act, Nothing)
    End Function


#Region "Constants"

    Public Const DiagnosticId = "String.Format Diagnostic"
    Public Const Description = "Is the formatstring valid?"
    Public Const MessageFormat = "Invalid FormatString (Reason: {0})"
    Public Const Category = "Validation"
#End Region

#Region "Rules"

    Public Rule1 As New DiagnosticDescriptor(id:=DiagnosticId,
                                             description:=Description,
                                             messageFormat:=MessageFormat,
                                             category:=Category,
                                             defaultSeverity:=DiagnosticSeverity.Warning)', isEnabledByDefault:=True)
    Public Rule2 As New DiagnosticDescriptor(id:=DiagnosticId,
                                             description:=Description,
                                             messageFormat:="This Constant is used as a FormatString" + Environment.NewLine + MessageFormat,
                                             category:=Category,
                                             defaultSeverity:=DiagnosticSeverity.Error)', isEnabledByDefault:=True)
    Public Rule3 As New DiagnosticDescriptor(id:=DiagnosticId,
                                             description:=Description,
                                             messageFormat:=MessageFormat,
                                             category:=Category,
                                             defaultSeverity:=DiagnosticSeverity.Error)', isEnabledByDefault:=True)
#End Region


    Public Function AddWarning(node As SyntaxNode, offset As Integer, endoffset As Integer, ri As IReportIssue) As Diagnostic
      Return Diagnostic.Create(Rule1,
                               Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), ri.Message)
    End Function

    Public Function AddError(node As SyntaxNode, offset As Integer, endoffset As Integer, ri As IReportIssue) As Diagnostic
      Return Diagnostic.Create(Rule3,
                               Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), ri.Message)
    End Function

    Public Function AddWarningAtSource(node As SyntaxNode, offset As Integer, endoffset As Integer, ri As IReportIssue) As Diagnostic
      Return Diagnostic.Create(Rule2,
                               Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), ri.Message)
    End Function

    Public Function AddErrorAtSource(node As SyntaxNode, offset As Integer, endoffset As Integer, ri As IReportIssue) As Diagnostic
      Return Diagnostic.Create(Rule2,
                               Location.Create(node.SyntaxTree, TextSpan.FromBounds(node.SpanStart + offset, node.SpanStart + endoffset)), ri.Message)
    End Function

    Public Function AddInformation(node As SyntaxNode, msg As String) As Diagnostic
      Return Diagnostic.Create(id:=DiagnosticId,
                               category:=Category, message:=String.Format(MessageFormat, msg),
                                     severity:=DiagnosticSeverity.Info,
                                     warningLevel:=0,
                                     isWarningAsError:=False, location:=Location.Create(node.SyntaxTree, node.Span))


      '      defaultSeverity:= DiagnosticSeverity.Info),
      'location:=Location.Create(node.SyntaxTree, node.Span),
      'severity:=DiagnosticSeverity.Info,
      'isEnabledByDefault:=True,
      'warningLevel:=0,
      'isWarningAsError:=False,
      'location:=Location.Create(node.SyntaxTree, node.Span))
    End Function

  End Module
End Namespace



