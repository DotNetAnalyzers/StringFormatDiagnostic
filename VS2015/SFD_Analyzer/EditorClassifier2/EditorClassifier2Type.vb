Imports System.ComponentModel.Composition
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities

Module EditorClassifier2TypeDef

  ''' <summary>
  ''' Defines the "EditorClassifier2" classification type.
  ''' </summary>
  <Export(GetType(ClassificationTypeDefinition))>
  <Name("EditorClassifier2")>
  Private _EditorClassifier2Type As ClassificationTypeDefinition

End Module
