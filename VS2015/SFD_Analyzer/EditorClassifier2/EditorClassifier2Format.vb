Imports System.ComponentModel.Composition
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities

''' <summary>
''' Defines an editor format for our EditorClassifier2 type that has a purple background
''' and is underlined.
''' </summary>
<Export(GetType(EditorFormatDefinition))>
<ClassificationType(ClassificationTypeNames:="EditorClassifier2")>
<Name("EditorClassifier2")>
<UserVisible(True)>
<Order(After:=Priority.Default)>
NotInheritable Class EditorClassifier2Format
  Inherits ClassificationFormatDefinition

  ''' <summary>
  ''' Defines the visual format for the "EditorClassifier2" classification type
  ''' </summary>
  Public Sub New()
    Me.DisplayName = "EditorClassifier2"
    Me.BackgroundColor = Colors.BlueViolet
    Me.BackgroundOpacity = 0.8
  End Sub

End Class
