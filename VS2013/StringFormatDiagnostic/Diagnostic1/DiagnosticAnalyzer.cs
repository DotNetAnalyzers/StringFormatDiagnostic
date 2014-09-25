using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Diagnostic1
{
    // TODO: Consider implementing other interfaces that implement IDiagnosticAnalyzer instead of or in addition to ISymbolAnalyzer

    [DiagnosticAnalyzer]
    [ExportDiagnosticAnalyzer(DiagnosticId, LanguageNames.CSharp)]
    public class DiagnosticAnalyzer : ISymbolAnalyzer
    {
        internal const string DiagnosticId = "Diagnostic1";
        internal const string Description = "Type name contains lowercase letters";
        internal const string MessageFormat = "Type name '{0}' contains lowercase letters";
        internal const string Category = "Naming";

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Description, MessageFormat, Category, DiagnosticSeverity.Warning);

        public ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create( Rule ); } }

        public ImmutableArray<SymbolKind> SymbolKindsOfInterest { get { return ImmutableArray.Create( SymbolKind.NamedType ); } }

        public void AnalyzeSymbol ( ISymbol symbol, Compilation compilation, Action<Diagnostic> addDiagnostic, CancellationToken cancellationToken )
        {
            // TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find

            var namedTypeSymbol = (INamedTypeSymbol)symbol;

            // Find just those named type symbols with names containing lowercase letters.
            if ( namedTypeSymbol.Name.Any( char.IsLower ) )
            {
                // For all such symbols, produce a diagnostic.
                var diagnostic = Diagnostic.Create(Rule, namedTypeSymbol.Locations[0], namedTypeSymbol.Name);

                addDiagnostic( diagnostic );
            }
        }
    }
}
