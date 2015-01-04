Imports System.Windows
Imports System.Windows.Controls
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text
Imports Microsoft.VisualStudio.Text.Editor
Imports Microsoft.VisualStudio.Text.Formatting
Imports Microsoft.CodeAnalysis.Text.Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic

''' <summary>
''' TextAdornment1 places red boxes behind all the "a"s in the editor window
''' </summary>
Class TextAdornment1

  Private WithEvents _view As IWpfTextView
  Private ReadOnly _layer As IAdornmentLayer
  Private ReadOnly _brush As Brush
  Private ReadOnly _pen As Pen

  Public Sub New(ByVal view As IWpfTextView)
    _view = view
    _layer = view.GetAdornmentLayer("TextAdornment1")

    'Create the pen and brush to color the box behind the a's
    Dim brush As New SolidColorBrush(Color.FromArgb(&H20, &H0, &H0, &HFF))
    brush.Freeze()
    Dim penBrush As New SolidColorBrush(Colors.Cornsilk)
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
    For Each line In e.NewOrReformattedLines
      Me.CreateVisuals(line)
    Next line
  End Sub

  Private Async Sub CreateVisuals(ByVal line As ITextViewLine)
    'grab a reference to the lines in the current TextView 
    Dim textViewLines = _view.TextViewLines
    Dim lineStart As Integer = line.Start
    Dim lineEnd As Integer = line.End

    Dim root = Await _view.TextSnapshot.GetOpenDocumentInCurrentContextWithChanges.GetSyntaxRootAsync
    Dim vbroot = DirectCast(root, VisualBasicSyntaxNode)
    Dim diags = vbroot.SyntaxTree.GetDiagnostics()

    For Each d In diags '.Where(Function(_d) _d.Id ="SFD000")
      Dim s = d.Location.SourceSpan.Start
      Dim e = d.Location.SourceSpan.End
      If (s >= lineStart) AndAlso (e <= lineEnd) Then
        Dim charSpan As New SnapshotSpan(_view.TextSnapshot, Span.FromBounds(s, e))
        Dim g As Geometry = textViewLines.GetMarkerGeometry(charSpan)
        If g Is Nothing Then Continue For
        Dim drawing As New GeometryDrawing(_brush, _pen, g)
        drawing.Freeze()

        Dim drawingImage As New DrawingImage(drawing)
        drawingImage.Freeze()

        Dim image As New Image() With {.Source = drawingImage}
        'Align the image with the top of the bounds of the text geometry
        Canvas.SetLeft(image, g.Bounds.Left)
        Canvas.SetTop(image, g.Bounds.Top)

        _layer.AddAdornment(AdornmentPositioningBehavior.TextRelative, charSpan,Nothing, image, Nothing)

      End If
    Next

  End Sub

End Class
