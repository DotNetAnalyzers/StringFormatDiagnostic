<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class Analyzer1Analyzer
  Inherits DiagnosticAnalyzer

  Public Const DiagnosticId = "Analyzer1"
  Friend Const Title = "Type name contains lowercase letters"
  Friend Const MessageFormat = "Type name '{0}' contains lowercase letters"
  Friend Const Category = "Naming"

  Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault:=True)

  Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
    Get
      Return ImmutableArray.Create(Rule)
    End Get
  End Property

  Public Overrides Sub Initialize(context As AnalysisContext)
    ' TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
    context.RegisterSymbolAction(AddressOf AnalyzeSymbol, SymbolKind.NamedType)
  End Sub

  Public Sub AnalyzeSymbol(context As SymbolAnalysisContext)
    ' TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find

    Dim namedTypeSymbol = CType(context.Symbol, INamedTypeSymbol)

    ' Find just those named type symbols with names containing lowercase letters.
    If namedTypeSymbol.Name.ToCharArray.Any(AddressOf Char.IsLower) Then
      ' For all such symbols, produce a diagnostic.
      Dim diag = Diagnostic.Create(Rule, namedTypeSymbol.Locations(0), namedTypeSymbol.Name)

      context.ReportDiagnostic(diag)
    End If
  End Sub
End Class