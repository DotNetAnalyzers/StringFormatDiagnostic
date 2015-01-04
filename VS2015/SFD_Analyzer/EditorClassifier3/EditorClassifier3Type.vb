Imports System.ComponentModel.Composition
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities

Module EditorClassifier3TypeDef

  ''' <summary>
  ''' Defines the "EditorClassifier3" classification type.
  ''' </summary>
  <Export(GetType(ClassificationTypeDefinition)),Name("SFD_000")>
  Private _SFD_000Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("SFD_001")>
  Private _SFD_001Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("SFD_002")>
  Private _SFD_002Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("SFD_003")>
  Private _SFD_003Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("SFD_004")>
  Private _SFD_004Type As ClassificationTypeDefinition

End Module
