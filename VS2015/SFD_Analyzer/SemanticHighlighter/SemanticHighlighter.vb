Imports System.ComponentModel.Composition
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities
Imports Microsoft.CodeAnalysis.Text.Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports DotNetAnalyzers.RoslynExts.VB
''' <summary>
''' This class causes a classifier to be added to the set of classifiers. Since 
''' the content type is set to "text", this classifier applies to all text files
''' </summary>
<Export(GetType(IClassifierProvider))>
<ContentType("Basic")>
Class SemanticHighlighterProvider
  Implements IClassifierProvider

  ''' <summary>
  ''' Import the classification registry to be used for getting a reference
  ''' to the custom classification type later.
  ''' </summary>
  <Import()>
  Private _classificationRegistry As IClassificationTypeRegistryService

  'returns an instance of the classifier
  Public Function GetClassifier(ByVal buffer As ITextBuffer) As IClassifier Implements IClassifierProvider.GetClassifier
    Return buffer.Properties.GetOrCreateSingletonProperty(Of SemanticHighlighter)(Function() New SemanticHighlighter(_classificationRegistry))
  End Function

End Class


''' <summary>
''' Classifier that classifies all spans as an instance of the SemanticHighlighterType
''' </summary>
Class SemanticHighlighter
  Implements IClassifier
  Private ReadOnly _classificationTypes As IClassificationType()

  Friend Sub New(ByVal registry As IClassificationTypeRegistryService)
    _classificationTypes = Enumerable.Range(0, 7).Select(Function(n) registry.GetClassificationType("Rainbow" & n)).ToArray
  End Sub


  ''' <summary>
  ''' This method scans the given SnapshotSpan for potential matches for this classification.
  ''' In this instance, it classifies everything and returns each span as a new ClassificationSpan.
  ''' </summary>
  ''' <param name="trackingSpan">The span currently being classified</param>
  ''' <returns>A list of ClassificationSpans that represent spans identified to be of this classification</returns>
  Public Function GetClassificationSpans(ByVal span As SnapshotSpan) As IList(Of ClassificationSpan) Implements IClassifier.GetClassificationSpans
    Dim classifications As New List(Of ClassificationSpan)
    Dim dict As New Dictionary(Of String, IClassificationType)
    Dim doc = span.Snapshot.GetOpenDocumentInCurrentContextWithChanges
    If doc Is Nothing Then Return classifications
    Dim sy = doc.GetSyntaxRootAsync.Result
    If sy Is Nothing Then Return classifications
    Dim sm = doc.GetSemanticModelAsync.Result
    If sm Is Nothing Then Return classifications
  
    'Dim n = sy.DescendantNodesAndSelf.Where(Function(sn) (sn.Span.Start >= span.Start.Position) AndAlso
    '                                             (sn.Span.End <= span.End.Position))
    'Dim ps = n.Where(Function(sn) (TypeOf sn Is Syntax.IdentifierNameSyntax) OrElse (TypeOf sn Is Syntax.ModifiedIdentifierSyntax))



    'Dim d = -1
    'Try
    '  For Each l In ps
    '    Dim v As IClassificationType = Nothing
    '    Dim t = ""
    '    If TypeOf l Is Syntax.IdentifierNameSyntax Then
    '      Dim p = l.Parent
    '      If TypeOf p Is Syntax.ParameterSyntax Then
    '        Dim nn = DirectCast(p, Syntax.ParameterSyntax)
    '        t = nn.Identifier.Identifier.Text
    '      ElseIf TypeOf p Is Syntax.AsClauseSyntax Then
    '        Continue For
    '      Else
    '       t = DirectCast(l,Syntax.IdentifierNameSyntax).Identifier.Text 
    '      End If

    '    ElseIf TypeOf l Is Syntax.ModifiedIdentifierSyntax Then
    '      Dim p = l.Parent
    '      If TypeOf p Is Syntax.ParameterSyntax Then
    '        Dim nn = DirectCast(p, Syntax.ParameterSyntax)
    '        t = nn.Identifier.Identifier.Text
    '      ElseIf TypeOf p Is Syntax.VariableDeclaratorSyntax Then
    '        Dim nn = DirectCast(p, Syntax.VariableDeclaratorSyntax)
    '        For Each tn In nn.Names
    '          Dim th=tn.Identifier.Text 
    '          If dict.TryGetValue(th, v) = False Then

    '            d = (d + 1) Mod _classificationTypes.Length
    '            v = _classificationTypes(d)
    '            dict.Add(th, v)
    '          End If
    '          classifications.Add(New ClassificationSpan(New SnapshotSpan(span.Snapshot, New Span(l.Span.Start, l.Span.Length)), v))

    '        Next
    '        Continue For 
    '      Else
    '        Continue For

    '      End If


    '    Else
    '      Continue For
    '    End If

    '    If dict.TryGetValue(t, v) = False Then

    '      d = (d + 1) Mod _classificationTypes.Length
    '      v = _classificationTypes(d)
    '      dict.Add(t, v)
    '    End If
    '    classifications.Add(New ClassificationSpan(New SnapshotSpan(span.Snapshot, New Span(l.Span.Start, l.Span.Length)), v))

    '  Next
    'Catch ex As Exception

    'End Try

    'Return classifications
  End Function

  ' This event gets raised if a non-text change would effect the classification in some way,
  ' for example typing /* would cause the classification to change in C# without directly
  ' affecting the span.
  Public Event ClassificationChanged As EventHandler(Of ClassificationChangedEventArgs) Implements IClassifier.ClassificationChanged

End Class


