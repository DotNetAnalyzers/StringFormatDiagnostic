using System;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Roslyn.StringFormatDiagnostics;
namespace StringFormatDiagnostic_CSharp
{
  [DiagnosticAnalyzer]
  [ExportDiagnosticAnalyzer(DiagnosticId , LanguageNames.CSharp)]
  public class DiagnosticAnalyzer : ISyntaxNodeAnalyzer<Microsoft.CodeAnalysis.CSharp.SyntaxKind>
  {
    internal const string DiagnosticId = "StringFormatDiagnostic_CSharp";
    public ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
    {
      get
      {
        return ImmutableArray.Create( Common.Rule1 , Common.Rule2 );
      }
    }
    ImmutableArray<SyntaxKind> ISyntaxNodeAnalyzer<SyntaxKind>.SyntaxKindsOfInterest
    {
      get
      {
        return ImmutableArray.Create( SyntaxKind.SimpleMemberAccessExpression );
      }
    }
    void ISyntaxNodeAnalyzer<SyntaxKind>.AnalyzeNode ( SyntaxNode node , SemanticModel semanticModel , Action<Diagnostic> addDiagnostic , CancellationToken cancellationToken )
    {
      var x = node as MemberAccessExpressionSyntax;
      if (x == null)
        return;
      switch (x.ToString().ToLower())
      {
      case "string.format":
        goto case "console.writeline";
      case "console.write":
        goto case "console.writeline";
      case "console.writeline":
        {
          var p = x.Parent as InvocationExpressionSyntax;
          var args = p.ArgumentList.Arguments;
          //var __ = string.Format( "A: {0}, B:={2}" , 1 , 1 );
          if (args.Count == 0)
            return; // error
          ArgumentSyntax fs = args.First();
          if (fs.IsMissing)
            return; // In vb this OmittedTypeArgumentSyntax
          var TheFormatString = fs.Expression;
          // as LiteralExpressionSyntax;
          if (TheFormatString != null)
          {
            var ck = TheFormatString.CSharpKind();
            if (ck == SyntaxKind.StringLiteralExpression)
            {
              var ReportedIssues = Common.AnalyseFormatString( cancellationToken , fs.ToString() , args.Count - 1 );
              foreach (var ReportedIssue in ReportedIssues)
              {
                if (ReportedIssue is ArgIndexOutOfRange)
                {
                  var cex = (ArgIndexOutOfRange)ReportedIssue;
                  addDiagnostic( Common.AddWarning( fs , cex.Start , 1 + cex.Finish , ReportedIssue ) );
                }
                else if (ReportedIssue is UnexpectedChar)
                {
                  var cex = (UnexpectedChar)ReportedIssue;
                  addDiagnostic( Common.AddWarning( fs , cex.Start , cex.Start + 1 , ReportedIssue ) );
                }
                else if (ReportedIssue is UnexpectedlyReachedEndOfText)
                {
                  var cex = (UnexpectedlyReachedEndOfText)ReportedIssue;
                  addDiagnostic( Common.AddWarning( fs , 0 , fs.Span.Length , ReportedIssue ) );
                }
                else if (ReportedIssue is ArgIndexHasExceedLimit)
                {
                  var cex = (ArgIndexHasExceedLimit)ReportedIssue;
                  addDiagnostic( Common.AddWarning( fs , cex.Start , 1 + cex.Finish , ReportedIssue ) );
                }
                else if (ReportedIssue is ContainsNoArgs)
                {
                  addDiagnostic( Common.AddInformation( fs , "Contains no args! Are you sure this Is correct?" ) );
                }
                else if (ReportedIssue is Internal_IssueReport)
                {
                  addDiagnostic( Common.AddWarning( node , 0 , fs.Span.Length , ReportedIssue ) );
                };
              }
            }
            else if (ck == SyntaxKind.IdentifierName)
            {
              var ThisIdentifier = TheFormatString as IdentifierNameSyntax;
              if (ThisIdentifier == null)
                return;
              var ConstValue = semanticModel.GetConstantValue( ThisIdentifier , cancellationToken );
              if (ConstValue.HasValue == false)
                return;
              var FoundSymbol = semanticModel.LookupSymbols( position: TheFormatString.Span.Start , name: ThisIdentifier.Identifier.Text ).ElementAt( 0 );
              var VariableDeclarationSite = ( FoundSymbol.DeclaringSyntaxReferences.ElementAt( 0 ).GetSyntax().Parent as VariableDeclaratorSyntax );
              if (VariableDeclarationSite == null)
                return;
              var TheValueOfTheVariable = VariableDeclarationSite.Initializer.Value;
              // Debugger.Break();
              if (FoundSymbol.IsExtern)
              {
                // Use usage site for location of Warings, ignore the yield ranges and use the span of ThisIdentifier.
                var ReportedIssues = Common.AnalyseFormatString( cancellationToken , ConstValue.Value.ToString() , args.Count - 1 );
                foreach (IssueReport ReportedIssue in ReportedIssues)
                {
                  if (ReportedIssue is ArgIndexOutOfRange)
                  {
                    var cex = (ArgIndexOutOfRange)ReportedIssue;
                    addDiagnostic( Common.AddWarningAtSource( fs , 0 , fs.Span.Length , ReportedIssue ) );
                  }
                  else if (ReportedIssue is UnexpectedChar)
                  {
                    var cex = (UnexpectedChar)ReportedIssue;
                    addDiagnostic( Common.AddWarningAtSource( fs , 0 , fs.Span.Length , ReportedIssue ) );
                  }
                  else if (ReportedIssue is UnexpectedlyReachedEndOfText)
                  {
                    var cex = (UnexpectedlyReachedEndOfText)ReportedIssue;
                    addDiagnostic( Common.AddWarningAtSource( fs , 0 , fs.Span.Length , ReportedIssue ) );
                  }
                  else if (ReportedIssue is ArgIndexHasExceedLimit)
                  {
                    var cex = (ArgIndexHasExceedLimit)ReportedIssue;
                    addDiagnostic( Common.AddWarningAtSource( fs , 0 , fs.Span.Length , ReportedIssue ) );
                  }
                  else if (ReportedIssue is ContainsNoArgs)
                  {
                    addDiagnostic( Common.AddInformation( fs , "Contains no args! Are you sure this Is correct?" ) );
                  }
                  else if (ReportedIssue is Internal_IssueReport)
                  {
                    addDiagnostic( Common.AddWarningAtSource( fs , 0 , fs.Span.Length , ReportedIssue ) );
                  };
                }
              }
              else
              {
                // Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                var ReportedIssues = Common.AnalyseFormatString( cancellationToken , ConstValue.Value.ToString() , args.Count - 1 );
                foreach (IssueReport ReportedIssue in ReportedIssues)
                {
                  if (ReportedIssue is ArgIndexHasExceedLimit)
                  {
                    var cex = (ArgIndexHasExceedLimit)ReportedIssue;
                    addDiagnostic( Common.AddWarning( TheValueOfTheVariable , cex.Start + 1 , 2 + cex.Finish , ReportedIssue ) );
                  }
                  else if (ReportedIssue is UnexpectedChar)
                  {
                    var cex = (UnexpectedChar)ReportedIssue;
                    addDiagnostic( Common.AddWarning( fs , cex.Start , cex.Start + 1 , ReportedIssue ) );
                  }
                  else if (ReportedIssue is UnexpectedlyReachedEndOfText)
                  {
                    var cex = (UnexpectedlyReachedEndOfText)ReportedIssue;
                    addDiagnostic( Common.AddWarning( fs , 0 , fs.Span.Length , ReportedIssue ) );
                  }
                  else if (ReportedIssue is ArgIndexHasExceedLimit)
                  {
                    var cex = (ArgIndexHasExceedLimit)ReportedIssue;
                    addDiagnostic( Common.AddWarning( fs , cex.Start , 1 + cex.Finish , ReportedIssue ) );
                  }
                  else if (ReportedIssue is ContainsNoArgs)
                  {
                    addDiagnostic( Common.AddWarningAtSource( TheValueOfTheVariable , 0 , TheValueOfTheVariable.Span.Length , ReportedIssue ) );
                  }
                  else if (ReportedIssue is Internal_IssueReport)
                  {
                    addDiagnostic( Common.AddWarningAtSource( TheValueOfTheVariable , 0 , TheValueOfTheVariable.Span.Length , ReportedIssue ) );
                  }
                }
              }
            }
          }
          break;
        }
      }
    }
  }
}