Imports Microsoft.CodeAnalysis.Diagnostics

Imports Microsoft.CodeAnalysis


Namespace Global.SFD.Analysis

  Namespace VB
    Public Interface IChecker

      Sub Check(method As VisualBasic.Syntax.MemberAccessExpressionSyntax,
                context As SyntaxNodeAnalysisContext,
                FIndex As Integer,
                Args As IEnumerable(Of Object))

    End Interface
  End Namespace


  Namespace CS
    Public Interface IChecker

      Sub Check(method As CSharp.Syntax.MemberAccessExpressionSyntax,
                context As SyntaxNodeAnalysisContext,
                FIndex As Integer,
                Args As IEnumerable(Of Object))

    End Interface
  End Namespace


End Namespace
