Imports System.Windows
Imports System.Windows.Controls
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text
Imports Microsoft.VisualStudio.Text.Editor
Imports Microsoft.VisualStudio.Text.Formatting
Imports Microsoft.CodeAnalysis.Text.Extensions
Imports Microsoft.VisualStudio.Text.Tagging
Imports SFD_Class
Imports System.ComponentModel.Composition
Imports Microsoft.VisualStudio.Utilities

''' <summary>
''' SFD_Class places red boxes behind all the "a"s in the editor window
''' </summary>
Class SFD_Class

  Private WithEvents _view As IWpfTextView
  Private ReadOnly _layer As IAdornmentLayer
  Private ReadOnly _brush As Brush
  Private ReadOnly _pen As Pen

  Public Sub New(ByVal view As IWpfTextView)
    _view = view
    _layer = view.GetAdornmentLayer("SFD_Class")
    Dim brush As New SolidColorBrush(Color.FromArgb(&H20, &H0, &H0, &HFF))
    brush.Freeze()
    Dim penBrush As New SolidColorBrush(Colors.Red)
    penBrush.Freeze()
    Dim pen As New Pen(penBrush, 0.5)
    pen.Freeze()

    _brush = brush
    _pen = pen
  End Sub

  ''' <summary>
  ''' On layout change add the adornment to any reformatted lines
  ''' </summary>
  Private Sub OnLayoutChanged(ByVal sender As Object, ByVal e As TextViewLayoutChangedEventArgs) Handles _view.LayoutChanged
    If e Is Nothing Then Exit Sub
    For Each line In e.NewOrReformattedLines
      Me.CreateVisuals(line)
    Next line
  End Sub

  Private Sub CreateVisuals(ByVal line As ITextViewLine)
    Try
      'grab a reference to the lines in the current TextView 
      Dim textViewLines = _view?.TextViewLines
      If textViewLines Is Nothing Then Exit Sub
      If line Is Nothing Then Exit Sub
      Dim lineStart As Integer = line.Start
      Dim lineEnd As Integer = line.End
      Dim q = textViewLines.FirstOrDefault
      If q Is Nothing Then Exit Sub
      Dim qq = q.Snapshot.GetOpenDocumentInCurrentContextWithChanges
      If qq Is Nothing Then Exit Sub
      Dim sm As Microsoft.CodeAnalysis.SemanticModel = Nothing
      If qq.TryGetSemanticModel(sm) = False Then Exit Sub
      '   Dim di = sm.GetSyntaxDiagnostics.ToArray
      If sm Is Nothing Then Exit Sub
      Dim diags = sm.GetDiagnostics.ToArray
      If diags.Any() = False Then Exit Sub
      For Each d In diags
        'If d.Id<>"SFD000" Then Continue For
        Dim charSpan As New SnapshotSpan(_view.TextSnapshot, Span.FromBounds(d.Location.SourceSpan.Start, d.Location.SourceSpan.End))
        Dim g As Geometry = textViewLines.GetMarkerGeometry(charSpan)
        If g IsNot Nothing Then
          Dim drawing As New GeometryDrawing(_brush, _pen, g) : drawing.Freeze()
          Dim drawingImage As New DrawingImage(drawing) : drawingImage.Freeze()
          Dim image As New Image() With {.Source = drawingImage}
          'Align the image with the top of the bounds of the text geometry
          Canvas.SetLeft(image, g.Bounds.Left)
          Canvas.SetTop(image, g.Bounds.Top)

          _layer?.AddAdornment(AdornmentPositioningBehavior.TextRelative, charSpan, Nothing, image, Nothing)
        End If
      Next
    Catch ex As Exception
      Debug.Print(ex.ToString)
    End Try
  End Sub

End Class

Class Tag_Base
  Implements ITag


End Class
'Class ArgHole
'  Inherits Tag_Base
'End Class

'Class Arg_Index
'  Inherits Tag_Base
'End Class
'Class Arg_Alignment
'  Inherits Tag_Base
'End Class
'Class Arg_Formatting
'  Inherits Tag_Base
'End Class
'Class Arg_Comma
'  Inherits Tag_Base
'End Class
'Class Arg_Colon
'  Inherits Tag_Base
'End Class
'Class Arg_Hole_Opening
'  Inherits Tag_Base
'End Class
'Class Arg_Hole_Closing
'  Inherits Tag_Base
'End Class

<Export(GetType(ITaggerProvider))>
<ContentType("Basic")>
<TagType(GetType(IClassificationTag))>
Class SFD_Tagger
  Implements ITagger(Of Tag_Base)

  Public Event TagsChanged As EventHandler(Of SnapshotSpanEventArgs) Implements ITagger(Of Tag_Base).TagsChanged

  Public Iterator Function GetTags(spans As NormalizedSnapshotSpanCollection) As IEnumerable(Of ITagSpan(Of Tag_Base)) Implements ITagger(Of Tag_Base).GetTags
    '  Throw New NotImplementedException()
    For Each _s_ In spans
      Dim doc = _s_.Snapshot.GetOpenDocumentInCurrentContextWithChanges
      If doc Is Nothing Then Continue For
      Dim st As Microsoft.CodeAnalysis.SyntaxTree = Nothing
      If doc.TryGetSyntaxTree(st) = False Then Continue For
      Dim diags = st.GetDiagnostics().Where(Function(d)
                                              Dim s = d.Location.SourceSpan
                                              Return (s.Start >= _s_.Start.Position) AndAlso (s.End <= _s_.End.Position)
                                            End Function)
      For Each di In diags
        Yield New TagSpan(Of Tag_Base)(New SnapshotSpan(_s_.Snapshot, di.Location.SourceSpan.Start, di.Location.SourceSpan.Length), Nothing)
      Next
    Next
  End Function


End Class

'Class SFD_
'  Implements ITaggerProvider

'  Public Function CreateTagger(Of T As ITag)(buffer As ITextBuffer) As ITagger(Of T) Implements ITaggerProvider.CreateTagger
'    Throw New NotImplementedExceptio
'    Dim doc  = buffer.CurrentSnapshot.GetOpenDocumentInCurrentContextWithChanges.
'  End Function
'End Class