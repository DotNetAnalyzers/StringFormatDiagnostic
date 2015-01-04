Imports System.ComponentModel.Composition
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities
Imports Microsoft.CodeAnalysis.Text. Extensions

''' <summary>
''' This class causes a classifier to be added to the set of classifiers. Since 
''' the content type is set to "text", this classifier applies to all text files
''' </summary>
<Export(GetType(IClassifierProvider))>
<ContentType("text")>
Class EditorClassifier2Provider
  Implements IClassifierProvider

  ''' <summary>
  ''' Import the classification registry to be used for getting a reference
  ''' to the custom classification type later.
  ''' </summary>
  <Import()>
  Private _classificationRegistry As IClassificationTypeRegistryService

  'returns an instance of the classifier
  Public Function GetClassifier(ByVal buffer As ITextBuffer) As IClassifier Implements IClassifierProvider.GetClassifier
    Return buffer.Properties.GetOrCreateSingletonProperty(Of EditorClassifier2)(Function() New EditorClassifier2(_classificationRegistry))
  End Function

End Class


''' <summary>
''' Classifier that classifies all spans as an instance of the EditorClassifier2Type
''' </summary>
Class EditorClassifier2
  Implements IClassifier

  Private ReadOnly _classificationType As IClassificationType

  Friend Sub New(ByVal registry As IClassificationTypeRegistryService)
    _classificationType = registry.GetClassificationType("EditorClassifier2")
  End Sub

  ''' <summary>
  ''' This method scans the given SnapshotSpan for potential matches for this classification.
  ''' In this instance, it classifies everything and returns each span as a new ClassificationSpan.
  ''' </summary>
  ''' <param name="trackingSpan">The span currently being classified</param>
  ''' <returns>A list of ClassificationSpans that represent spans identified to be of this classification</returns>
  Public Function GetClassificationSpans(ByVal span As SnapshotSpan) As IList(Of ClassificationSpan) Implements IClassifier.GetClassificationSpans

    'create a list to hold the results
    Dim classifications As New List(Of ClassificationSpan)
    Dim doc = span.Snapshot.GetOpenDocumentInCurrentContextWithChanges 
  If doc Is Nothing Then Return classifications
    Dim st As Microsoft.CodeAnalysis.SyntaxNode = Nothing
    If doc.TryGetSyntaxRoot(st) = False Then Return classifications
    Dim diags = st.GetDiagnostics.ToArray
    'Where(Function(d) 
    '                                    Dim l=d.Location
    '                                    Dim q= span.Span 
    '                                    Return (l.Start>= span.Start.Position ) AndAlso (l.End<=span.End.Position  )
    '                                   End Function )
    For Each di In diags
      classifications.Add(New ClassificationSpan(
           New SnapshotSpan(span.Snapshot, New Span(di.Location.SourceSpan.Start, di.Location.SourceSpan.Length)),
           _classificationType))
    Next

    Return classifications
  End Function

  ' This event gets raised if a non-text change would effect the classification in some way,
  ' for example typing /* would cause the classification to change in C# without directly
  ' affecting the span.
  Public Event ClassificationChanged As EventHandler(Of ClassificationChangedEventArgs) Implements IClassifier.ClassificationChanged

End Class
