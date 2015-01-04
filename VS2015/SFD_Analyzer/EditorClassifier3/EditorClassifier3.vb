Imports System.ComponentModel.Composition
Imports System.Windows.Media
Imports Microsoft.VisualStudio.Text
Imports Microsoft.VisualStudio.Text.Classification
Imports Microsoft.VisualStudio.Utilities
Imports Microsoft.CodeAnalysis.Text.Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasicExtensions
Imports System.Runtime.CompilerServices
Imports SFD
Imports SFD.StringFormat.Exts
Imports SFD.StringFormat
Imports SFD.StringFormat.StringFormat
Imports System.Threading.Tasks
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.Diagnostics
Imports RoslynExts
Imports RoslynExts.VB

''' <summary>
''' This class causes a classifier to be added to the set of classifiers. Since 
''' the content type is set to "text", this classifier applies to all text files
''' </summary>
<Export(GetType(IClassifierProvider))>
<ContentType("Basic")>
Class EditorClassifier3Provider
  Implements IClassifierProvider

  ''' <summary>
  ''' Import the classification registry to be used for getting a reference
  ''' to the custom classification type later.
  ''' </summary>
  <Import()>
  Private _classificationRegistry As IClassificationTypeRegistryService

  'returns an instance of the classifier
  Public Function GetClassifier(ByVal buffer As ITextBuffer) As IClassifier Implements IClassifierProvider.GetClassifier
    Return buffer.Properties.GetOrCreateSingletonProperty(Of EditorClassifier3)(Function() New EditorClassifier3(_classificationRegistry))
  End Function

End Class


''' <summary>
''' Classifier that classifies all spans as an instance of the EditorClassifier3Type
''' </summary>
Class EditorClassifier3
  Implements IClassifier

  Private ReadOnly _ArgHoleContents_ As IClassificationType
  Private ReadOnly _ArgHoleBraces_ As IClassificationType
  Private ReadOnly _Digitsx_ As IClassificationType
  Private ReadOnly _Format_ As IClassificationType
  Private ReadOnly _Error_ As IClassificationType



  Friend Sub New(ByVal registry As IClassificationTypeRegistryService)

    SFD.Analysers.Initialise()
    _ArgHoleContents_ = registry.GetClassificationType("SFD_000")
    _ArgHoleBraces_ = registry.GetClassificationType("SFD_001")
    _Digitsx_ = registry.GetClassificationType("SFD_002")
    _Format_ = registry.GetClassificationType("SFD_003")
    _Error_ = registry.GetClassificationType("SFD_004")
  End Sub

  Dim SameMethod As Func(Of SFD.Analysis.SFD_Diag, String, MemberAccessExpressionSyntax, Boolean) =
    Function(a, calledOn, thisMethodAccess)
      Dim p0 = (String.Compare(a.TypeName, calledOn, True) = 0)
      Dim p1 = (String.Compare(a.MethodName, thisMethodAccess.Name.Identifier.Text, True) = 0)
      Return p0 AndAlso p1
    End Function


  ''' <summary>
  ''' This method scans the given SnapshotSpan for potential matches for this classification.
  ''' In this instance, it classifies everything and returns each span as a new ClassificationSpan.
  ''' </summary>
  ''' <param name="trackingSpan">The span currently being classified</param>
  ''' <returns>A list of ClassificationSpans that represent spans identified to be of this classification</returns>
  Public Function GetClassificationSpans(ByVal span As SnapshotSpan) As IList(Of ClassificationSpan) Implements IClassifier.GetClassificationSpans
    Dim classifications As New List(Of ClassificationSpan)()
    Dim thisSnapshot = span.Snapshot : If thisSnapshot.TextBuffer.EditInProgress Then Return classifications
    Dim doc = thisSnapshot.GetOpenDocumentInCurrentContextWithChanges : If doc Is Nothing Then Return classifications
    Dim st As SyntaxTree = Nothing : If doc.TryGetSyntaxTree(st) = False Then Return classifications
    Dim root As SyntaxNode = Nothing : If st.TryGetRoot(root) = False Then Return classifications

    Dim AvailableColorisers = SFD.Analysers.Analysis.AsParallel.Where(Function(a) a.Coloriser.Length > 0)
    Dim methodInvokes = root.DescendantNodesAndSelf.AsParallel.OfType(Of MemberAccessExpressionSyntax).
                             Where(Function(thisMethodAccess) thisMethodAccess.OperatorToken.IsKind(SyntaxKind.DotToken)).
                             Where(Function(x) (x.Span.Start >= span.Start.Position) AndAlso (x.Span.End <= span.End.Position))

    Dim rev = methodInvokes.AsParallel.
      Where(Function(thisMethodAccess) As Boolean
              Dim _InvokeExpr_ = thisMethodAccess.Parent.Try(of InvocationExpressionSyntax)
              If _InvokeExpr_ Is Nothing Then Return False
              Dim calledOn = thisMethodAccess.CalledOnType(doc).ToFullyQualifiedName
              Dim DiagAnalysers = AvailableColorisers.Where(Function(a) SameMethod(a,calledOn,thisMethodAccess))
              Return DiagAnalysers.Any()
            End Function)

    For Each mi In rev.AsParallel
      Dim _Invoke_ = mi.Parent.As(Of InvocationExpressionSyntax) : If _Invoke_ Is Nothing Then Continue For
      Dim calledOn = mi.CalledOnType(doc).ToFullyQualifiedName
      Dim Coloriser = AvailableColorisers.FirstOrDefault(Function(a) SameMethod(a, calledOn, mi))
      If Coloriser Is Nothing Then Continue For
      Dim ArgLis = _Invoke_.ArgumentList : If ArgLis Is Nothing Then Continue For 
      Dim Args = ArgLis.Arguments
      If (Coloriser.FIndex < 0) OrElse (Coloriser.FIndex >= Args.Count) Then Continue For
      Dim _Arg_ = Args(Coloriser.FIndex)
      If _Arg_ Is Nothing Then Continue For
      If TypeOf _Arg_ Is OmittedArgumentSyntax Then Continue For
      Dim TheFormatString = _Arg_.As(Of  SimpleArgumentSyntax)
      Select Case TheFormatString.Expression.VBKind
        Case SyntaxKind.StringLiteralExpression : Parse_FormatString(TheFormatString, classifications, span.Snapshot)
        Case SyntaxKind.IdentifierName
      End Select
    Next
    Return classifications
  End Function


  Sub Parse_FormatString(fs As Syntax.ArgumentSyntax, classifications As List(Of ClassificationSpan), snap As ITextSnapshot)
    If fs Is Nothing Then Exit Sub
    If classifications Is Nothing Then Exit Sub
    Dim TheArg = fs.GetText().ToString()
    If TheArg.StartsWith(""""c) AndAlso TheArg.EndsWith(""""c) Then
      TheArg = TheArg.Trim(""""c)
      Dim TheSource = SourceText.Create(TheArg)
      Dim Result = StringFormat.Parse(TheSource)
      If Result.MadeOf IsNot Nothing Then
        For Each arg In Result.MadeOf
          If arg Is Nothing Then Continue For
          Select Case arg.Kind
            Case Kinds.ArgHole : ColorIn_Arg(fs, arg, classifications, snap)
            Case Kinds.Err_Malformed_ArgHole,
                 Kinds.Err_Malformed_Text : classifications.Add(snap.MakeClassSpan(fs, arg.Start, arg.Finish, _Error_))
            Case Kinds.Err_UC, Kinds.Err_UnknownSpecifier, Kinds.Err_SpecifierUnknown
              classifications.Add(snap.MakeClassSpan(fs, arg.Start, arg.Finish, _Error_))

          End Select
        Next
      End If

    End If

  End Sub


  Private Sub ColorIn_Arg(fs As SyntaxNode, sk As SpanKind, classifications As List(Of ClassificationSpan), snap As ITextSnapshot)
    If (fs Is Nothing) AndAlso (sk Is Nothing) Then Exit Sub
    If classifications Is Nothing Then Exit Sub
    If sk.MadeOf Is Nothing Then Exit Sub
    For Each p In sk.MadeOf
      If p Is Nothing Then Continue For
      Select Case p.Kind
        Case Kinds.BL,
             Kinds.BR,
             Kinds.Comma,
             Kinds.Colon : classifications.Add(snap.MakeClassSpan(fs, p.Start, p.Finish, _ArgHoleBraces_))
        Case Kinds.Arg_Index : ColorIn_ArgIndex(fs, p, classifications, snap)
        Case Kinds.Arg_Align : ColorIn_ArgAlign(fs, p, classifications, snap)
        Case Kinds.Arg_Format : ColorIn_ArgFormat(fs, p, classifications, snap)
        Case Else
      End Select
    Next
  End Sub

  Private Sub ColorIn_ArgIndex(fs As SyntaxNode, sk As SpanKind, classifications As List(Of ClassificationSpan), snap As ITextSnapshot)
    classifications.Add(snap.MakeClassSpan(fs, sk.Start, sk.Finish, _Digitsx_))
    If (sk Is Nothing) OrElse (sk.MadeOf Is Nothing) Then Exit Sub
    For Each p In sk.MadeOf
      If p Is Nothing Then Continue For
      Select Case p.Kind
        Case Kinds.Digits : classifications.Add(snap.MakeClassSpan(fs, p.Start, p.Finish, _Digitsx_))
      End Select
    Next

  End Sub

  Private Sub ColorIn_ArgAlign(fs As SyntaxNode, sk As SpanKind, classifications As List(Of ClassificationSpan), snap As ITextSnapshot)
    If (sk Is Nothing) OrElse (sk.MadeOf Is Nothing) Then Exit Sub
    For Each p In sk.MadeOf
      If p Is Nothing Then Continue For
      Select Case p.Kind
        Case Kinds.Comma : classifications.Add(snap.MakeClassSpan(fs, p.Start, p.Finish, _ArgHoleBraces_))
        Case Kinds.Digits,
             Kinds.Minus : classifications.Add(snap.MakeClassSpan(fs, p.Start, p.Finish, _Digitsx_))
      End Select
    Next
  End Sub

  Private Sub ColorIn_ArgFormat(fs As SyntaxNode, sk As SpanKind, classifications As List(Of ClassificationSpan), snap As ITextSnapshot)
    If (sk Is Nothing) OrElse (sk.MadeOf) Is Nothing Then Exit Sub
    classifications.Add(snap.MakeClassSpan(fs, sk.Start, sk.Finish, _Format_))
    For Each p In sk.MadeOf
      If p Is Nothing Then Continue For
      Select Case p.Kind
        Case Kinds.Err_SpecifierUnknown,
             Kinds.Err_TooManySections,
             Kinds.Err_UC,
             Kinds.Err_UnknownSpecifier,
             Kinds.Err_ValueExceedsLimit
                   classifications.Add(snap.MakeClassSpan(fs, p.Start, p.Finish, _Error_))

        Case Kinds.Colon :  classifications.Add(snap.MakeClassSpan(fs, sk.Start, sk.Finish, _ArgHoleBraces_))
        Case Kinds.TextChars : classifications.Add(snap.MakeClassSpan(fs, sk.Start + 1, sk.Finish, _Format_))
      End Select
    Next
  End Sub

  ' This event gets raised if a non-text change would effect the classification in some way,
  ' for example typing /* would cause the classification to change in C# without directly
  ' affecting the span.
  Public Event ClassificationChanged As EventHandler(Of ClassificationChangedEventArgs) Implements IClassifier.ClassificationChanged


End Class

Public Module Exts

  <Extension>
  Public Function CalledOnType(n As MemberAccessExpressionSyntax, context As SyntaxNodeAnalysisContext) As INamedTypeSymbol
    If (n Is Nothing) Then Return Nothing
    Dim s = context.SemanticModel.GetSymbolInfo(n, context.CancellationToken).Symbol
    Return If(s Is Nothing, Nothing, s.ContainingType)
  End Function
  <Extension>
  Public Function CalledOnType(n As MemberAccessExpressionSyntax, sm As SemanticModel) As INamedTypeSymbol
    If (n Is Nothing) Then Return Nothing
    Dim s = sm.GetSymbolInfo(n).Symbol
    Return If(s Is Nothing, Nothing, s.ContainingType)
  End Function
  <Extension>
  Public Function CalledOnType(n As MemberAccessExpressionSyntax, doc As Document) As ISymbol 'INamedTypeSymbol
    If (n Is Nothing) Then Return Nothing
    Dim q = Microsoft.CodeAnalysis.FindSymbols.SymbolFinder.FindSymbolAtPositionAsync(doc, n.SpanStart).Result
    Dim s = q ' .GetSymbolInfo(n).Symbol
    Return s'If(s Is Nothing, Nothing, s.ContainingType)
  End Function
  <Extension>
  Public Function ToFullyQualifiedName(s As ISymbol) As String
    If s Is Nothing Then Return String.Empty
    Return s.ToDisplayString(New SymbolDisplayFormat(typeQualificationStyle:=SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces))
  End Function

  <Extension>
  Public Function MakeClassSpan(ss As ITextSnapshot, fs As SyntaxNode, p1 As Integer, p2 As Integer, _color_ As IClassificationType) As ClassificationSpan
    Dim size = p2 - p1
    Return New ClassificationSpan(New SnapshotSpan(New SnapshotPoint(ss, fs.SpanStart + p1 + 1), size - 0), _color_)
  End Function
  Public Function MakeClassSpan(ss As ITextSnapshot, fs As Syntax.ArgumentSyntax, p1 As Integer, _color_ As IClassificationType) As ClassificationSpan
    Return MakeClassSpan(ss, fs, p1, p1, _color_)
  End Function

End Module
