Option Strict On
Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.CSharp.Syntax
Imports Roslyn.StringFormatDiagnostics.CSharp.Exts
Imports Common
Imports Roslyn.StringFormatDiagnostics.CSharp


<DiagnosticAnalyzer>
<ExportDiagnosticAnalyzer(DiagnosticId, LanguageNames.CSharp)>
Public Class DiagnosticAnalyzer
    Implements ISyntaxNodeAnalyzer(Of Microsoft.CodeAnalysis.CSharp.SyntaxKind)

    Public ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor) Implements IDiagnosticAnalyzer.SupportedDiagnostics
        Get
            Return ImmutableArray.Create(Rule1, Rule2)
        End Get
    End Property
    Private ReadOnly Property SyntaxKindsOfInterest As ImmutableArray(Of CSharp.SyntaxKind) Implements ISyntaxNodeAnalyzer(Of CSharp.SyntaxKind).SyntaxKindsOfInterest
        Get
            Return ImmutableArray.Create(CSharp.SyntaxKind.SimpleMemberAccessExpression)
        End Get
    End Property


    Public Sub AnalyzeNode(node As SyntaxNode, semanticModel As SemanticModel, addDiagnostic As Action(Of Diagnostic), options As AnalyzerOptions, cancellationToken As CancellationToken) Implements ISyntaxNodeAnalyzer(Of CSharp.SyntaxKind).AnalyzeNode

        Dim x = CType(node, MemberAccessExpressionSyntax)
        If x Is Nothing Then Exit Sub
        If x.OperatorToken.ValueText = "." Then
      Dim _MethodName = x.Name.ToString
      If _MethodName = "" Then Exit Sub
      Dim ct = x.CalledOnType(semanticModel, cancellationToken)
      Dim _NameOfType = If(ct Is Nothing, "", ct.ToFullyQualifiedName)
      Dim invokation = TryCast(x.Parent, InvocationExpressionSyntax)
      If invokation Is Nothing Then Exit Sub
      Dim Args = invokation.ArgumentList

      Dim ArgTypes = Args.GetArgumentTypes(semanticModel, cancellationToken)
      Dim ArgTypeNames = Args.GetArgumentTypesNames(semanticModel, cancellationToken)
      'Dim FullyNamed = ArgTypes(1).ToFullyQualifiedName
      'Dim GottenType = Type.GetType(FullyNamed, False, True)
      ''Dim Obj = GottenType.BuildMeOne '.GetConstructors()(0).Invoke({Nothing})
      'Dim ov = Convert.ChangeType(Args.Arguments(1).ToString, GottenType)

      'Debugger.Break 

      Select Case _NameOfType
        Case "System.Console"
          Select Case _MethodName
            Case "Write", "WriteLine" : DoValidation(x, semanticModel, addDiagnostic, cancellationToken)
          End Select
        Case "System.String"
          Select Case _MethodName
            Case "Format" : DoValidation(x, semanticModel, addDiagnostic, cancellationToken)
          End Select
        Case "System.Text.StringBuilder"
          Select Case _MethodName
            Case "AppendFormat"
              Select Case Args.Arguments.Count
                Case 0, 1 : Exit Sub
                Case 2, 3, 4
                  If ArgTypeNames(0) = "System.String" Then DoValidation(x, semanticModel, addDiagnostic, cancellationToken)

                Case Else
                  If ArgTypeNames(0) = "System.String" Then DoValidation(x, semanticModel, addDiagnostic, cancellationToken)
              End Select
          End Select
        Case Else
          'Select Case _MethodName
          '  Case "ToString"
          '    Select Case _NameOfType
          '      Case "System.Integer"

          '    End Select
          '    'Select Case Args.Arguments.Count
          '    '  Case 1
          '    '    Select Case ArgTypeNames(0)
          '    '      Case "String"
          '    '      Case "IFormatProvider"
          '    '        'Dim FullyNamed = ArgTypes(0).ToFullyQualifiedName
          '    '        'Dim GottenType = Type.GetType(FullyNamed, False, True)
          '    '        'Dim Obj = GottenType.BuildMeOne'.GetConstructors()(0).Invoke({Nothing})

          '    '        'Debugger.breAK
          '    '    End Select
          '    '  Case 2
          '    '    Dim ii = ArgTypes(1).Interfaces.Where(Function(i) i.Name = "IFormatProvider").FirstOrDefault
          '    '    If ii IsNot Nothing Then Exit Sub
          '    '  '             Dim ir = semanticModel.GetSymbolInfo (ArgTypes(1),cancellationToken ).S 
          '    '  'If ArgTypeNames(0) = "String" AndAlso  Then

          '    '  '    End If
          '    '  Case Else
          '    '    Exit Sub
          '    'End Select
          'End Select
      End Select
    End If
    End Sub


    Public Sub DoValidation(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
        Dim p = CType(node.Parent, InvocationExpressionSyntax)


        Dim args = p.ArgumentList.Arguments
        Select Case args.Count
            Case 0 ' Error
            Case Else
                Dim fs = args.First
                If fs.IsMissing Then Exit Sub
                Dim TheFormatString = CType(fs, ArgumentSyntax)
                If TheFormatString IsNot Nothing Then
                    Select Case TheFormatString.Expression.CSharpKind
                        Case CSharp.SyntaxKind.StringLiteralExpression
                            Dim ReportedIssues = AnalyseFormatString(ct, fs.ToString, args.Count - 1, p.ArgumentList.GetArgumentAsObjects(sm, ct).Skip(1).ToArray)
                            For Each ReportedIssue In ReportedIssues
                                Select Case True
                                    Case TypeOf ReportedIssue Is ArgIndexOutOfRange
                                        Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
                                        addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                                    Case TypeOf ReportedIssue Is UnexpectedChar
                                        Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
                                        addDiagnostic(AddWarning(fs, cex.Start, cex.Start + 1, ReportedIssue))
                                    Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText
                                        addDiagnostic(AddWarning(fs, 0, fs.Span.Length, ReportedIssue))
                                    Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit
                                        Dim cex = DirectCast(ReportedIssue, ArgIndexHasExceedLimit)
                                        addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                                    Case TypeOf ReportedIssue Is FinalOutput
                                        addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                                    Case TypeOf ReportedIssue Is ContainsNoArgs
                                        addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                                    Case TypeOf ReportedIssue Is ContainsNoParameters
                                        addDiagnostic(AddInformation(fs, "No parameters! Are you sure this Is correct?"))
                                    Case TypeOf ReportedIssue Is Internal_IssueReport
                                        addDiagnostic(AddWarning(node, 0, fs.Span.Length, ReportedIssue))
                                End Select
                            Next
                        Case CSharp.SyntaxKind.IdentifierName
                            Dim ThisIdentifier = CType(TheFormatString.Expression, IdentifierNameSyntax)
                            If ThisIdentifier Is Nothing Then Exit Sub
                            Dim ConstValue = sm.GetConstantValue(ThisIdentifier, ct)
                            If ConstValue.HasValue = False Then Exit Sub
                            Dim FoundSymbol = sm.LookupSymbols(TheFormatString.Expression.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
                            Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, VariableDeclaratorSyntax)
                            If VariableDeclarationSite Is Nothing Then Exit Sub
                            Dim TheValueOfTheVariable = VariableDeclarationSite.Initializer.Value
                            'Debugger.Break()
                            If FoundSymbol.IsExtern Then
                                ' Use usage site for location of Warings, ignore the yield ranges and use the span of ThisIdentifier.
                                Dim ReportedIssues = AnalyseFormatString(ct, ConstValue.Value.ToString, args.Count - 1, p.ArgumentList.GetArgumentAsObjects(sm, ct).Skip(1).ToArray)
                                For Each ReportedIssue In ReportedIssues
                                    Select Case True
                                        Case TypeOf ReportedIssue Is ArgIndexOutOfRange
                                            addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                                        Case TypeOf ReportedIssue Is UnexpectedChar
                                            addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                                        Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText
                                            addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                                        Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit
                                            addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                                        Case TypeOf ReportedIssue Is FinalOutput
                                            addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                                        Case TypeOf ReportedIssue Is ContainsNoArgs
                                            addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                                        Case TypeOf ReportedIssue Is ContainsNoParameters
                                            addDiagnostic(AddInformation(fs, "No parameters! Are you sure this Is correct?"))
                                        Case TypeOf ReportedIssue Is Internal_IssueReport
                                            addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                                    End Select
                                Next
                            Else
                                ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                                Dim ReportedIssues = AnalyseFormatString(ct, ConstValue.Value.ToString, args.Count - 1, p.ArgumentList.GetArgumentAsObjects(sm, ct).Skip(1).ToArray)
                                For Each ReportedIssue In ReportedIssues
                                    Select Case True
                                        Case TypeOf ReportedIssue Is ArgIndexOutOfRange
                                            Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
                                            addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                                        Case TypeOf ReportedIssue Is UnexpectedChar
                                            Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
                                            addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, cex.Start + 2, ReportedIssue))
                                        Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText
                                            addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                                        Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit
                                            Dim cex = DirectCast(ReportedIssue, ArgIndexHasExceedLimit)
                                            addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                                        Case TypeOf ReportedIssue Is FinalOutput
                                            addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                                        Case TypeOf ReportedIssue Is ContainsNoArgs
                                            addDiagnostic(AddInformation(TheValueOfTheVariable, "Contains no args! Are you sure this Is correct?"))
                                        Case TypeOf ReportedIssue Is ContainsNoParameters
                                            addDiagnostic(AddInformation(TheValueOfTheVariable, "No parameters! Are you sure this Is correct?"))
                                        Case TypeOf ReportedIssue Is Internal_IssueReport
                                            addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                                    End Select
                                Next
                            End If
                    End Select
                End If
        End Select

    End Sub
End Class