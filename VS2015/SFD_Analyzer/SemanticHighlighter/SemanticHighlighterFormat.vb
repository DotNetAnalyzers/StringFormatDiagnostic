Imports System.ComponentModel.Composition
Imports System.Windows
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities


<Export(GetType(EditorFormatDefinition))>
<ClassificationType(ClassificationTypeNames:="Rainbow0")>
<Name("Rainbow0")>
<UserVisible(True)>
<Order(After:=Priority.High)>
NotInheritable Class Rainbow0
  Inherits ClassificationFormatDefinition

  Public Sub New()
    Me.DisplayName = "Rainbow0"
    Me.ForegroundColor = Colors.Red
  End Sub

End Class


<Export(GetType(EditorFormatDefinition))>
<ClassificationType(ClassificationTypeNames:="Rainbow1")>
<Name("Rainbow1")>
<UserVisible(True)>
<Order(After:=Priority.High)>
NotInheritable Class Rainbow1
  Inherits ClassificationFormatDefinition

  Public Sub New()
    Me.DisplayName = "Rainbow1"
    Me.ForegroundColor = Colors.Orange
  End Sub

End Class


<Export(GetType(EditorFormatDefinition))>
<ClassificationType(ClassificationTypeNames:="Rainbow2")>
<Name("Rainbow2")>
<UserVisible(True)>
<Order(After:=Priority.High)>
NotInheritable Class Rainbow2
  Inherits ClassificationFormatDefinition

  Public Sub New()
    Me.DisplayName = "Rainbow2"
    Me.ForegroundColor = Colors.Yellow
  End Sub

End Class


<Export(GetType(EditorFormatDefinition))>
<ClassificationType(ClassificationTypeNames:="Rainbow3")>
<Name("Rainbow3")>
<UserVisible(True)>
<Order(After:=Priority.High)>
NotInheritable Class Rainbow3
  Inherits ClassificationFormatDefinition

  Public Sub New()
    Me.DisplayName = "Rainbow3"
    Me.ForegroundColor = Colors.Green
  End Sub

End Class


<Export(GetType(EditorFormatDefinition))>
<ClassificationType(ClassificationTypeNames:="Rainbow4")>
<Name("Rainbow4")>
<UserVisible(True)>
<Order(After:=Priority.High)>
NotInheritable Class Rainbow4
  Inherits ClassificationFormatDefinition

  Public Sub New()
    Me.DisplayName = "Rainbow4"
    Me.ForegroundColor = Colors.Blue
  End Sub

End Class


<Export(GetType(EditorFormatDefinition))>
<ClassificationType(ClassificationTypeNames:="Rainbow5")>
<Name("Rainbow5")>
<UserVisible(True)>
<Order(After:=Priority.High)>
NotInheritable Class Rainbow5
  Inherits ClassificationFormatDefinition

  Public Sub New()
    Me.DisplayName = "Rainbow5"
    Me.ForegroundColor = Colors.Indigo
  End Sub

End Class


<Export(GetType(EditorFormatDefinition))>
<ClassificationType(ClassificationTypeNames:="Rainbow6")>
<Name("Rainbow6")>
<UserVisible(True)>
<Order(After:=Priority.High)>
NotInheritable Class Rainbow6
  Inherits ClassificationFormatDefinition

  Public Sub New()
    Me.DisplayName = "Rainbow6"
    Me.ForegroundColor = Colors.Violet
  End Sub

End Class
