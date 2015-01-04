Imports System.ComponentModel.Composition
Imports System.Windows
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities

Module Constants
  Public Const R0 = "Rainbow0"
  Public Const R1 = "Rainbow1"
  Public Const R2 = "Rainbow2"
  Public Const R3 = "Rainbow3"
  Public Const R4 = "Rainbow4"
  Public Const R5 = "Rainbow5"
  Public Const R6 = "Rainbow6"
End Module

Class RainbowBase
  Inherits ClassificationFormatDefinition
  Public Sub New(Name As String, fg As Color)
    MyBase.New
    Me.DisplayName = Name
    Me.ForegroundColor = fg
  End Sub
End Class

<UserVisible(True),Order(After:=Priority.High), Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=R0), Name(R0)>
NotInheritable Class Rainbow0
  Inherits RainbowBase 
  Public Sub New()
    MyBase.New(R0,Colors.Red)
  End Sub
End Class


<UserVisible(True),Order(After:=Priority.High), Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=R1),Name(R1)>
NotInheritable Class Rainbow1
  Inherits RainbowBase 

  Public Sub New()
    Mybase.New(R1, Colors.Orange)
  End Sub

End Class


<UserVisible(True),Order(After:=Priority.High), Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=R2),Name(R2)>
NotInheritable Class Rainbow2
  Inherits RainbowBase
  Public Sub New()
    MyBase.New(R2, Colors.Gold)
  End Sub

End Class
<UserVisible(True),Order(After:=Priority.High), Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=R3), Name(R3)>
NotInheritable Class Rainbow3
  Inherits RainbowBase
  Public Sub New()
    MyBase.New(R3, Colors.Green )
  End Sub

End Class
<UserVisible(True),Order(After:=Priority.High), Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=R4), Name(R4)>
NotInheritable Class Rainbow4
  Inherits RainbowBase
  Public Sub New()
    MyBase.New(R4, Colors.Blue)
  End Sub

End Class
<UserVisible(True),Order(After:=Priority.High), Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=R5), Name(R5)>
NotInheritable Class Rainbow5
  Inherits RainbowBase
  Public Sub New()
    MyBase.New(R5, Colors.Indigo)
  End Sub

End Class
<UserVisible(True),Order(After:=Priority.High), Export(GetType(EditorFormatDefinition)), ClassificationType(ClassificationTypeNames:=R6), Name(R6)>
NotInheritable Class Rainbow6
  Inherits RainbowBase
  Public Sub New()
    MyBase.New(R6, Colors.Violet)
  End Sub

End Class



'<ClassificationType(ClassificationTypeNames:="Rainbow3")>Name("Rainbow3")>
'<UserVisible(True)>
'<Order(After:=Priority.High)>
'NotInheritable Class Rainbow3
'  Inherits ClassificationFormatDefinition

'  Public Sub New()
'    Me.DisplayName = "Rainbow3"
'    Me.ForegroundColor = Colors.Green 
'  End Sub

'End Class


'<Export(GetType(EditorFormatDefinition))>
'<ClassificationType(ClassificationTypeNames:="Rainbow4")>
'<Name("Rainbow4")>
'<UserVisible(True)>
'<Order(After:=Priority.High)>
'NotInheritable Class Rainbow4
'  Inherits ClassificationFormatDefinition

'  Public Sub New()
'    Me.DisplayName = "Rainbow4"
'    Me.ForegroundColor = Colors.Blue 
'  End Sub

'End Class


'<Export(GetType(EditorFormatDefinition))>
'<ClassificationType(ClassificationTypeNames:="Rainbow5")>
'<Name("Rainbow5")>
'<UserVisible(True)>
'<Order(After:=Priority.High)>
'NotInheritable Class Rainbow5
'  Inherits ClassificationFormatDefinition

'  Public Sub New()
'    Me.DisplayName = "Rainbow5"
'    Me.ForegroundColor = Colors.Indigo 
'  End Sub

'End Class


'<Export(GetType(EditorFormatDefinition)),ClassificationType(ClassificationTypeNames:="Rainbow6"),
'Name("Rainbow6"),UserVisible(True),Order(After:=Priority.High)>
'NotInheritable Class Rainbow6
'  Inherits ClassificationFormatDefinition

'  Public Sub New()
'    Me.DisplayName = "Rainbow6"
'    Me.ForegroundColor = Colors.Violet 
'  End Sub

'End Class
