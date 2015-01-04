Imports System.ComponentModel.Composition
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities

Module RainbowTypeDefs

  ''' <summary>
  ''' Defines the "EditorClassifier1" classification type.
  ''' </summary>
  <Export(GetType(ClassificationTypeDefinition)), Name("Rainbow0")>
  Private _Rainbow0Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("Rainbow1")>
  Private _Rainbow1Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("Rainbow2")>
  Private _Rainbow2Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("Rainbow3")>
  Private _Rainbow3Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("Rainbow4")>
  Private _Rainbow4Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("Rainbow5")>
  Private _Rainbow5Type As ClassificationTypeDefinition
  <Export(GetType(ClassificationTypeDefinition)), Name("Rainbow6")>
  Private _Rainbow6Type As ClassificationTypeDefinition
End Module
