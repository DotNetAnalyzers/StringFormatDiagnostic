Imports System.ComponentModel.Composition
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities

Module Constants
  Const Public _000 = "SFD_000"
  Public Const _001 = "SFD_001"
  Public Const _002 = "SFD_002"
  Public Const _003 = "SFD_003"
  Public Const _004 = "SFD_004"
End Module

<Export(GetType(EditorFormatDefinition)),ClassificationType(ClassificationTypeNames:=_000)>
<Name(_000), UserVisible(True), Order(After:=Priority.High)>
NotInheritable Class SFD_000
  Inherits ClassificationFormatDefinition

  ''' <summary>
  ''' Defines the visual format for the "EditorClassifier3" classification type
  ''' </summary>
  Public Sub New()
    Me.DisplayName = _000
    Me.BackgroundColor = Colors.LightSlateGray
    Me.BackgroundOpacity = 0.5
    Me.IsItalic = true
    'Me.TextDecorations = System.Windows.TextDecorations.Underline
  End Sub

End Class

<Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=_001)>
<Name(_001), UserVisible(True), Order(After:=Priority.High)>
NotInheritable Class SFD_001
  Inherits ClassificationFormatDefinition

  ''' <summary>
  ''' Defines the visual format for the "EditorClassifier3" classification type
  ''' </summary>
  Public Sub New()
    Me.DisplayName = _001
    Me.ForegroundColor = Colors.Black
    Me.ForegroundOpacity = 1
    Me.IsItalic = True
    Me.IsBold = True
  End Sub

End Class

<Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=_002)>
<Name(_002), UserVisible(True), Order(After:=Priority.High)>
NotInheritable Class SFD_002
  Inherits ClassificationFormatDefinition

  ''' <summary>
  ''' Defines the visual format for the "EditorClassifier3" classification type
  ''' </summary>
  Public Sub New()
    Me.DisplayName = _002
    '    Me.BackgroundColor = Colors.LightSalmon
    '    Me.BackgroundOpacity = 0.6
    Me.ForegroundColor = Colors.SlateBlue 
    Me.ForegroundOpacity = 1
    Me.IsItalic = True
    Me.IsBold = True
    'Me.TextDecorations = System.Windows.TextDecorations.Underline
  End Sub

End Class

<Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=_003)>
<Name(_003), UserVisible(True), Order(After:=Priority.High)>
NotInheritable Class SFD_003
  Inherits ClassificationFormatDefinition

  ''' <summary>
  ''' Defines the visual format for the "EditorClassifier3" classification type
  ''' </summary>
  Public Sub New()
    Me.DisplayName = _003
    'Me.BackgroundColor = Colors.LightSalmon
    'Me.BackgroundOpacity = 0.6
    Me.ForegroundColor = Colors.LightSalmon
    Me.ForegroundOpacity = 1
    Me.IsItalic = True
    Me.IsBold = True
    'Me.TextDecorations = System.Windows.TextDecorations.Underline
  End Sub

End Class

<Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=_004)>
<Name(_004), UserVisible(True), Order(After:=Priority.High)>
NotInheritable Class SFD_004
  Inherits ClassificationFormatDefinition

  ''' <summary>
  ''' Defines the visual format for the "EditorClassifier3" classification type
  ''' </summary>
  Public Sub New()
    Me.DisplayName = _004
    Me.BackgroundColor = Colors.Plum 
    Me.BackgroundOpacity  = 0.25
  End Sub

End Class