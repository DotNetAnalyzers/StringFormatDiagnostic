Imports System.ComponentModel.Composition
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities
Imports Microsoft.CodeAnalysis.Text.Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports System.Linq.Enumerable


''' <summary>
''' This class causes a classifier to be added to the set of classifiers. Since 
''' the content type is set to "text", this classifier applies to all text files
''' </summary>
<Export(GetType(IClassifierProvider)), ContentType("text")>
Class EditorClassifier1Provider
  Implements IClassifierProvider

  ''' <summary>
  ''' Import the classification registry to be used for getting a reference
  ''' to the custom classification type later.
  ''' </summary>
  <Import()>
  Private _classificationRegistry As IClassificationTypeRegistryService

  'returns an instance of the classifier
  Public Function GetClassifier(ByVal buffer As ITextBuffer) As IClassifier Implements IClassifierProvider.GetClassifier
    Return buffer.Properties.GetOrCreateSingletonProperty(Of RainbowBraces)(Function() New RainbowBraces(_classificationRegistry))
  End Function

End Class


''' <summary>
''' Classifier that classifies all spans as an instance of the EditorClassifier1Type
''' </summary>
Class RainbowBraces
  Implements IClassifier

  Private ReadOnly _Coloring As IClassificationType()
  Private _Depth As Integer =-1

  Friend Sub New(ByVal registry As IClassificationTypeRegistryService)
    _Depth = -1
    _Coloring = Enumerable.Range(0, 7).Select(Function(n) registry.GetClassificationType("Rainbow" & n)).ToArray
  End Sub

  ''' <summary>
  ''' This method scans the given SnapshotSpan for potential matches for this classification.
  ''' In this instance, it classifies everything and returns each span as a new ClassificationSpan.
  ''' </summary>
  ''' <param name="span">The span currently being classified</param>
  ''' <returns>A list of ClassificationSpans that represent spans identified to be of this classification</returns>
  Public Function GetClassificationSpans(ByVal span As SnapshotSpan) As IList(Of ClassificationSpan) Implements IClassifier.GetClassificationSpans

    Dim classifications As New List(Of ClassificationSpan)
    If span.Snapshot Is Nothing Then Return classifications 
    Dim doc = span.Snapshot.GetOpenDocumentInCurrentContextWithChanges
    If doc Is Nothing Then Return classifications 
    Dim myRoot As SyntaxNode = Nothing
    If doc.TryGetSyntaxRoot(myRoot) Then
      Dim InSpanTokens = myRoot.DescendantTokens.Where(Function(sn) (sn.Span.Start >= span.Start.Position) AndAlso (sn.Span.End <= span.End.Position))
      Dim Braces = InSpanTokens.Where(Function(sn) sn.KindIsAnyOf(SyntaxKind.CloseParenToken, SyntaxKind.OpenParenToken,
                                                                  SyntaxKind.OpenBraceToken,  SyntaxKind.CloseBraceToken))
      For Each Brace In Braces
        If Brace.KindIsAnyOf(SyntaxKind.OpenParenToken,SyntaxKind.OpenBraceToken) Then _Depth += 1
        If (_Depth = _Coloring.Length) Then _Depth = 0
        classifications.Add(New ClassificationSpan(span.ToSnapshotSpan(Brace), _Coloring(_Depth)))
        If Brace.KindIsAnyOf(SyntaxKind.CloseParenToken,SyntaxKind.CloseBraceToken) Then _Depth -= 1
        If (_Depth < 0) Then _Depth = _Coloring.Length - 1
      Next
    End If
    Return classifications
  End Function


  Public Event ClassificationChanged As EventHandler(Of ClassificationChangedEventArgs) Implements IClassifier.ClassificationChanged

End Class

Public Module Exts
  <Runtime.CompilerServices.Extension>
  Public Function KindIsAnyOf(value As SyntaxToken, ParamArray values() As SyntaxKind) As Boolean
    Return values.Any(Function(v) value.IsKind(v))
  End Function
end Module
Public Module VS_Exts
  <Runtime.CompilerServices.Extension>
  Public Function ToSpan(token As SyntaxToken) As Microsoft.VisualStudio.Text.Span
    Return New Span(token.Span.Start, token.Span.Length)
  End Function
  <Runtime.CompilerServices.Extension>
  Public Function ToSnapshotSpan(snap As SnapshotSpan,token As SyntaxToken) As SnapshotSpan
    Return New SnapshotSpan(snap.Snapshot, token.ToSpan)
  End Function
End Module