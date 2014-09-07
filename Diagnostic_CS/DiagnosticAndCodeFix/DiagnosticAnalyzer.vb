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
      Dim _CalledOnObjOfType = x.CalledOnType(semanticModel, cancellationToken)
      Dim _TypeName = If(_CalledOnObjOfType Is Nothing, "", _CalledOnObjOfType.ToFullyQualifiedName)
      Dim _TypeNameA() = _TypeName.Split("."c)
      Dim _InvokeExpr = TryCast(x.Parent, InvocationExpressionSyntax)
      If _InvokeExpr Is Nothing Then Exit Sub
      Dim Args = _InvokeExpr.ArgumentList

      Dim ArgTypes = Args.GetArgumentTypes(semanticModel, cancellationToken)
      Dim ArgTypeNames = Args.GetArgumentTypesNames(semanticModel, cancellationToken)
      ' Try to see if it is one the simple ones
      Dim res = From tns In Common.TheSimpleOnes
                Where tns.TypeName = _TypeName
                Select tns.MethodNames
      If res.Any Then
        If res(0).Any(Function(mn) mn = _MethodName) Then DoValidation(x, semanticModel, addDiagnostic, cancellationToken)
      Else
        ' Try and see if it is one the more complex options
        Select Case _TypeName
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
            ' Finally let's see if we can validate the .ToString(" ", ) methods
            Select Case _TypeName
              Case "System.Int32", "System.Int16", "System.Int64", "System.UInt32", "System.UInt16", "System.UInt64",
                   "System.Byte", "System.UByte",
                   "System.Double", "System.Single",
                   "System.Decimal"
                Select Case _MethodName
                  Case "ToString"
                    Select Case Args.Arguments.Count
                      Case 1 : Check_Numeric_ToString(x, semanticModel, addDiagnostic, cancellationToken)
                    End Select
                End Select

              Case "System.DateTime"
                Select Case _MethodName
                  Case "ToString"
                    Select Case Args.Arguments.Count
                      Case 1 : Check_DateTime_ToString(x, semanticModel, addDiagnostic, cancellationToken)
                    End Select
                End Select
              Case "System.TimeSpan"
                Select Case _MethodName
                  Case "ToString"
                    Select Case Args.Arguments.Count
                      Case 1 : Check_TimeSpan_ToString(x, semanticModel, addDiagnostic, cancellationToken)
                    End Select
                End Select
              Case "System.DateTimeOffset"
                Select Case _MethodName
                  Case "ToString"
                    Select Case Args.Arguments.Count
                      Case 1 : Check_DateTimeOffset_ToString(x, semanticModel, addDiagnostic, cancellationToken)
                    End Select
                End Select
              Case "System.Enum"
                Select Case _MethodName
                  Case "ToString"
                    Select Case Args.Arguments.Count
                      Case 1 : Check_Enum_ToString(x, semanticModel, addDiagnostic, cancellationToken)
                    End Select
                End Select
            End Select
        End Select

      End If
    End If

  End Sub
  Private Sub _Shared_Checker_(fn As Func(Of CancellationToken, String, IFormatProvider, OutputResult(Of String)), node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
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
            Case SyntaxKind.StringLiteralExpression
              Dim ReportedIssues = fn(ct, Common.DeString(fs.ToString), Nothing)
              For Each ReportedIssue In ReportedIssues.Errors
                Select Case True
                  Case TypeOf ReportedIssue Is UnexpectedChar
                    Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
                    addDiagnostic(AddWarning(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                  Case TypeOf ReportedIssue Is IgnoredChar
                    Dim cex = DirectCast(ReportedIssue, IgnoredChar)
                    addDiagnostic(AddWarning(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                  Case TypeOf ReportedIssue Is TooManySections
                    Dim cex = DirectCast(ReportedIssue, TooManySections)
                    addDiagnostic(AddWarning(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                  Case TypeOf ReportedIssue Is UnknownSpecifier
                    Dim cex = DirectCast(ReportedIssue, UnknownSpecifier)
                    addDiagnostic(AddWarning(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                  Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText : addDiagnostic(AddWarning(fs, 0, fs.Span.Length, ReportedIssue))
                  Case TypeOf ReportedIssue Is ValueHasExceedLimit
                    Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
                    addDiagnostic(AddWarningAtSource(fs, cex.Start + 1, 1 + ((cex.Finish + 1) - cex.Start), ReportedIssue))
                  Case TypeOf ReportedIssue Is Internal_Information : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                  Case TypeOf ReportedIssue Is FinalOutput : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                  Case TypeOf ReportedIssue Is Internal_IssueReport : addDiagnostic(AddWarning(node, 0, fs.Span.Length, ReportedIssue))
                End Select
              Next
            Case SyntaxKind.IdentifierName
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
                Dim ReportedIssues = fn(ct, ConstValue.Value.ToString, Nothing)
                For Each ReportedIssue In ReportedIssues.Errors
                  Select Case True
                    Case TypeOf ReportedIssue Is ArgIndexOutOfRange : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is UnexpectedChar : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is IgnoredChar : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is TooManySections
                      Dim cex = DirectCast(ReportedIssue, TooManySections)
                      addDiagnostic(AddWarning(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                    Case TypeOf ReportedIssue Is ValueHasExceedLimit
                      Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
                      addDiagnostic(AddWarningAtSource(fs, cex.Start + 1, 1 + ((cex.Finish + 1) - cex.Start), ReportedIssue))
                    Case TypeOf ReportedIssue Is FinalOutput : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                    Case TypeOf ReportedIssue Is ContainsNoArgs : addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                    Case TypeOf ReportedIssue Is ContainsNoParameters : addDiagnostic(AddInformation(fs, "No parameters! Are you sure this Is correct?"))
                    Case TypeOf ReportedIssue Is Internal_IssueReport : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is Internal_Information : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                  End Select
                Next
              Else
                ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                Dim ReportedIssues = fn(ct, ConstValue.Value.ToString, Nothing)
                For Each ReportedIssue In ReportedIssues.Errors
                  Select Case True
                    Case TypeOf ReportedIssue Is ArgIndexOutOfRange
                      Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                    Case TypeOf ReportedIssue Is UnexpectedChar
                      Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
                      addDiagnostic(AddWarningAtSource(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                    Case TypeOf ReportedIssue Is IgnoredChar
                      Dim cex = DirectCast(ReportedIssue, IgnoredChar)
                      addDiagnostic(AddWarningAtSource(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                    Case TypeOf ReportedIssue Is TooManySections
                      Dim cex = DirectCast(ReportedIssue, TooManySections)
                      addDiagnostic(AddWarningAtSource(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                    Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText
                      Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit
                      Dim cex = DirectCast(ReportedIssue, ArgIndexHasExceedLimit)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                    Case TypeOf ReportedIssue Is ValueHasExceedLimit
                      Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
                      addDiagnostic(AddWarningAtSource(fs, cex.Start + 1, 1 + ((cex.Finish + 1) - cex.Start), ReportedIssue))
                    Case TypeOf ReportedIssue Is FinalOutput
                      Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
                      addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                    Case TypeOf ReportedIssue Is ContainsNoArgs
                      addDiagnostic(AddInformation(TheValueOfTheVariable, "Contains no args! Are you sure this Is correct?"))
                    Case TypeOf ReportedIssue Is ContainsNoParameters
                      addDiagnostic(AddInformation(TheValueOfTheVariable, "No parameters! Are you sure this Is correct?"))
                    Case TypeOf ReportedIssue Is Internal_IssueReport
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is Internal_Information
                      addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                    Case TypeOf ReportedIssue Is SpecifierUnkown
                      Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
                      addDiagnostic(AddWarningAtSource(fs, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                  End Select
                Next
              End If
          End Select
        End If
    End Select
  End Sub


  Public Sub Check_TimeSpan_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    _Shared_Checker_(AddressOf Analyse_TimeSpan_ToString, node, sm, addDiagnostic, ct)
  End Sub

  Public Sub Check_Enum_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    _Shared_Checker_(AddressOf Analyse_Enum_ToString, node, sm, addDiagnostic, ct)
  End Sub

  Public Sub Check_DateTimeOffset_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    _Shared_Checker_(AddressOf Analyse_DateTimeOffset_ToString, node, sm, addDiagnostic, ct)
  End Sub



  Public Sub Check_DateTime_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    _Shared_Checker_(AddressOf Analyse_DateTime_ToString, node, sm, addDiagnostic, ct)
  End Sub

  Public Sub Check_Numeric_ToString(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
    _Shared_Checker_(AddressOf Analyse_Numeric_ToString, node, sm, addDiagnostic, ct)
  End Sub


  'Public Sub DoValidation(node As MemberAccessExpressionSyntax, sm As SemanticModel, addDiagnostic As Action(Of Diagnostic), ct As CancellationToken)
  '  Dim p = CType(node.Parent, InvocationExpressionSyntax)
  '  Dim args = p.ArgumentList.Arguments
  '  Select Case args.Count
  '    Case 0 ' Error
  '    Case Else
  '      Dim fs = args.First
  '      If TypeOf fs Is OmittedArgumentSyntax Then Exit Sub
  '      Dim TheFormatString = CType(fs, SimpleArgumentSyntax)
  '      If TheFormatString IsNot Nothing Then
  '        Select Case TheFormatString.Expression.VisualBasicKind
  '          Case SyntaxKind.StringLiteralExpression
  '            Dim ReportedIssues = AnalyseFormatString(ct, fs.ToString, args.Count - 1, p.ArgumentList.GetArgumentAsObjects(sm, ct).Skip(1).ToArray)
  '            For Each ReportedIssue In ReportedIssues.Errors
  '              'Select Case ReportedIssue
  '              '    Case cex As ArgIndexOutOfRange : addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
  '              '    Case cex As UnexpectedChar : addDiagnostic(AddWarning(fs, cex.Start, cex.Start + 1, ReportedIssue))
  '              '    Case cex As UnexpectedlyReachedEndOfText : addDiagnostic(AddWarning(fs, 0, fs.Span.Length, ReportedIssue))
  '              '    Case cex As ArgIndexHasExceedLimit : addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
  '              '    Case ___ As FinalOutput : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
  '              '    Case ___ As ContainsNoArgs : addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
  '              '    Case ___ As ContainsNoParameters : addDiagnostic(AddInformation(fs, "No parameters! Are you sure this Is correct?"))
  '              '    Case ___ As Internal_IssueReport : addDiagnostic(AddWarning(node, 0, fs.Span.Length, ReportedIssue))
  '              'End Select
  '              Select Case True
  '                Case TypeOf ReportedIssue Is ArgIndexOutOfRange
  '                  Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
  '                  addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
  '                Case TypeOf ReportedIssue Is UnexpectedChar
  '                  Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
  '                  addDiagnostic(AddWarning(fs, cex.Start, cex.Start + 1, ReportedIssue))
  '                Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText
  '                  addDiagnostic(AddWarning(fs, 0, fs.Span.Length, ReportedIssue))
  '                Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit
  '                  Dim cex = DirectCast(ReportedIssue, ArgIndexHasExceedLimit)
  '                  addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
  '                Case TypeOf ReportedIssue Is ValueHasExceedLimit
  '                  Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
  '                  addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
  '                Case TypeOf ReportedIssue Is FinalOutput
  '                  addDiagnostic(AddInformation(fs, ReportedIssue.Message))
  '                Case TypeOf ReportedIssue Is ContainsNoArgs
  '                  addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
  '                Case TypeOf ReportedIssue Is ContainsNoParameters
  '                  addDiagnostic(AddInformation(fs, "No parameters! Are you sure this Is correct?"))
  '                Case TypeOf ReportedIssue Is Internal_IssueReport
  '                  addDiagnostic(AddWarning(node, 0, fs.Span.Length, ReportedIssue))
  '                Case TypeOf ReportedIssue Is Internal_Information
  '                  addDiagnostic(AddInformation(fs, ReportedIssue.Message))
  '              End Select
  '            Next
  '          Case SyntaxKind.IdentifierName
  '            Dim ThisIdentifier = CType(TheFormatString.Expression, IdentifierNameSyntax)
  '            If ThisIdentifier Is Nothing Then Exit Sub
  '            Dim ConstValue = sm.GetConstantValue(ThisIdentifier, ct)
  '            If ConstValue.HasValue = False Then Exit Sub
  '            Dim FoundSymbol = sm.LookupSymbols(TheFormatString.Expression.Span.Start, name:=ThisIdentifier.Identifier.Text)(0)
  '            Dim VariableDeclarationSite = TryCast(FoundSymbol.DeclaringSyntaxReferences(0).GetSyntax.Parent, VariableDeclaratorSyntax)
  '            If VariableDeclarationSite Is Nothing Then Exit Sub
  '            Dim TheValueOfTheVariable = VariableDeclarationSite.Initializer.Value
  '            'Debugger.Break()
  '            If FoundSymbol.IsExtern Then
  '              ' Use usage site for location of Warings, ignore the yield ranges and use the span of ThisIdentifier.
  '              Dim ReportedIssues = AnalyseFormatString(ct, ConstValue.Value.ToString, args.Count - 1, p.ArgumentList.GetArgumentAsObjects(sm, ct).Skip(1).ToArray)
  '              For Each ReportedIssue In ReportedIssues.Errors
  '                Select Case True
  '                  Case TypeOf ReportedIssue Is ArgIndexOutOfRange : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is UnexpectedChar : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is ValueHasExceedLimit : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is FinalOutput : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
  '                  Case TypeOf ReportedIssue Is ContainsNoArgs : addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
  '                  Case TypeOf ReportedIssue Is ContainsNoParameters : addDiagnostic(AddInformation(fs, "No parameters! Are you sure this Is correct?"))
  '                  Case TypeOf ReportedIssue Is Internal_IssueReport : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is Internal_Information : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
  '                End Select
  '              Next
  '            Else
  '              ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
  '              Dim ReportedIssues = AnalyseFormatString(ct, ConstValue.Value.ToString, args.Count - 1, p.ArgumentList.GetArgumentAsObjects(sm, ct).Skip(1).ToArray)
  '              For Each ReportedIssue In ReportedIssues.Errors
  '                Select Case True
  '                  Case TypeOf ReportedIssue Is ArgIndexOutOfRange
  '                    Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
  '                    addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is UnexpectedChar
  '                    Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
  '                    addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, cex.Start + 2, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is UnexpectedChar
  '                    Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
  '                    addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, cex.Start + 2, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText
  '                    Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
  '                    addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit
  '                    Dim cex = DirectCast(ReportedIssue, ArgIndexHasExceedLimit)
  '                    addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is ValueHasExceedLimit
  '                    Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
  '                    addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is FinalOutput
  '                    Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
  '                    addDiagnostic(AddInformation(fs, ReportedIssue.Message))
  '                  Case TypeOf ReportedIssue Is ContainsNoArgs
  '                    addDiagnostic(AddInformation(TheValueOfTheVariable, "Contains no args! Are you sure this Is correct?"))
  '                  Case TypeOf ReportedIssue Is ContainsNoParameters
  '                    addDiagnostic(AddInformation(TheValueOfTheVariable, "No parameters! Are you sure this Is correct?"))
  '                  Case TypeOf ReportedIssue Is Internal_IssueReport
  '                    addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
  '                  Case TypeOf ReportedIssue Is Internal_Information
  '                    addDiagnostic(AddInformation(fs, ReportedIssue.Message))
  '                End Select
  '              Next
  '            End If
  '        End Select
  '      End If
  '  End Select

  'End Sub


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
            Case SyntaxKind.StringLiteralExpression
              Dim ReportedIssues = AnalyseFormatString(ct, fs.ToString, args.Count - 1, p.ArgumentList.GetArgumentAsObjects(sm, ct).Skip(1).ToArray)
              For Each ReportedIssue In ReportedIssues.Errors
                'Select Case ReportedIssue
                '    Case cex As ArgIndexOutOfRange : addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                '    Case cex As UnexpectedChar : addDiagnostic(AddWarning(fs, cex.Start, cex.Start + 1, ReportedIssue))
                '    Case cex As UnexpectedlyReachedEndOfText : addDiagnostic(AddWarning(fs, 0, fs.Span.Length, ReportedIssue))
                '    Case cex As ArgIndexHasExceedLimit : addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                '    Case ___ As FinalOutput : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                '    Case ___ As ContainsNoArgs : addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                '    Case ___ As ContainsNoParameters : addDiagnostic(AddInformation(fs, "No parameters! Are you sure this Is correct?"))
                '    Case ___ As Internal_IssueReport : addDiagnostic(AddWarning(node, 0, fs.Span.Length, ReportedIssue))
                'End Select
                Select Case True
                  Case TypeOf ReportedIssue Is ArgIndexOutOfRange
                    Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
                    addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                  Case TypeOf ReportedIssue Is UnexpectedChar
                    Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
                    addDiagnostic(AddWarning(fs, cex.Start, cex.Start + 1, ReportedIssue))
                  Case TypeOf ReportedIssue Is IgnoredChar
                    Dim cex = DirectCast(ReportedIssue, IgnoredChar)
                    addDiagnostic(AddWarning(fs, cex.Start, cex.Start + 1, ReportedIssue))
                  Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText
                    addDiagnostic(AddWarning(fs, 0, fs.Span.Length, ReportedIssue))
                  Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit
                    Dim cex = DirectCast(ReportedIssue, ArgIndexHasExceedLimit)
                    addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                  Case TypeOf ReportedIssue Is ValueHasExceedLimit
                    Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
                    addDiagnostic(AddWarning(fs, cex.Start, 1 + cex.Finish, ReportedIssue))
                  Case TypeOf ReportedIssue Is FinalOutput
                    addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                  Case TypeOf ReportedIssue Is ContainsNoArgs
                    addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                  Case TypeOf ReportedIssue Is ContainsNoParameters
                    addDiagnostic(AddInformation(fs, "No parameters! Are you sure this Is correct?"))
                  Case TypeOf ReportedIssue Is Internal_IssueReport
                    addDiagnostic(AddWarning(node, 0, fs.Span.Length, ReportedIssue))
                  Case TypeOf ReportedIssue Is Internal_Information
                    addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                  Case TypeOf ReportedIssue Is TooManySections
                    Dim cex = DirectCast(ReportedIssue, TooManySections)
                    addDiagnostic(AddWarning(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                  Case TypeOf ReportedIssue Is SpecifierUnkown
                    Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
                    addDiagnostic(AddWarning(fs, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                End Select
              Next
            Case SyntaxKind.IdentifierName
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
                For Each ReportedIssue In ReportedIssues.Errors
                  Select Case True
                    Case TypeOf ReportedIssue Is ArgIndexOutOfRange : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is UnexpectedChar : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is ValueHasExceedLimit : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is FinalOutput : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                    Case TypeOf ReportedIssue Is ContainsNoArgs : addDiagnostic(AddInformation(fs, "Contains no args! Are you sure this Is correct?"))
                    Case TypeOf ReportedIssue Is ContainsNoParameters : addDiagnostic(AddInformation(fs, "No parameters! Are you sure this Is correct?"))
                    Case TypeOf ReportedIssue Is Internal_IssueReport : addDiagnostic(AddWarningAtSource(fs, 0, fs.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is Internal_Information : addDiagnostic(AddInformation(fs, ReportedIssue.Message))
                    Case TypeOf ReportedIssue Is SpecifierUnkown
                      Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
                      addDiagnostic(AddWarningAtSource(fs, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                    Case TypeOf ReportedIssue Is TooManySections
                      Dim cex = DirectCast(ReportedIssue, TooManySections)
                      addDiagnostic(AddWarningAtSource(fs, cex.Start + 1, cex.Start + 2, ReportedIssue))
                  End Select
                Next
              Else
                ' Use the declaration site location ( SpanOfConstantValue ) for the location of the warnings. Also use the yield ranges for the highlighting.              
                Dim ReportedIssues = AnalyseFormatString(ct, ConstValue.Value.ToString, args.Count - 1, p.ArgumentList.GetArgumentAsObjects(sm, ct).Skip(1).ToArray)
                For Each ReportedIssue In ReportedIssues.Errors
                  Select Case True
                    Case TypeOf ReportedIssue Is ArgIndexOutOfRange
                      Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                    Case TypeOf ReportedIssue Is UnexpectedChar
                      Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, cex.Start + 2, ReportedIssue))
                    Case TypeOf ReportedIssue Is IgnoredChar
                      Dim cex = DirectCast(ReportedIssue, IgnoredChar)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, cex.Start + 2, ReportedIssue))
                    Case TypeOf ReportedIssue Is UnexpectedChar
                      Dim cex = DirectCast(ReportedIssue, UnexpectedChar)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, cex.Start + 2, ReportedIssue))
                    Case TypeOf ReportedIssue Is UnexpectedlyReachedEndOfText
                      Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is ArgIndexHasExceedLimit
                      Dim cex = DirectCast(ReportedIssue, ArgIndexHasExceedLimit)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                    Case TypeOf ReportedIssue Is ValueHasExceedLimit
                      Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                    Case TypeOf ReportedIssue Is SpecifierUnkown
                      Dim cex = DirectCast(ReportedIssue, ValueHasExceedLimit)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, 2 + cex.Finish, ReportedIssue))
                    Case TypeOf ReportedIssue Is FinalOutput
                      Dim cex = DirectCast(ReportedIssue, ArgIndexOutOfRange)
                      addDiagnostic(AddInformation(TheValueOfTheVariable, ReportedIssue.Message))
                    Case TypeOf ReportedIssue Is ContainsNoArgs
                      addDiagnostic(AddInformation(TheValueOfTheVariable, "Contains no args! Are you sure this Is correct?"))
                    Case TypeOf ReportedIssue Is ContainsNoParameters
                      addDiagnostic(AddInformation(TheValueOfTheVariable, "No parameters! Are you sure this Is correct?"))
                    Case TypeOf ReportedIssue Is Internal_IssueReport
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, 0, TheValueOfTheVariable.Span.Length, ReportedIssue))
                    Case TypeOf ReportedIssue Is Internal_Information
                      addDiagnostic(AddInformation(TheValueOfTheVariable, ReportedIssue.Message))
                    Case TypeOf ReportedIssue Is TooManySections
                      Dim cex = DirectCast(ReportedIssue, TooManySections)
                      addDiagnostic(AddWarningAtSource(TheValueOfTheVariable, cex.Start + 1, cex.Start + 2, ReportedIssue))
                  End Select
                Next
              End If
          End Select
        End If
    End Select

  End Sub
End Class