Option Strict On
Imports System.Threading
Imports AdamSpeight2008.StringFormatDiagnostic
Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces
Imports AdamSpeight2008.StringFormatDiagnostic.IssueReports
Imports System.Runtime.CompilerServices

Namespace Global.AdamSpeight2008.StringFormatDiagnostic.Common
<HideModuleName> 
 Public Module VSCommon
    Private _Analysis As New List(Of SFD_Diag)
    Private _IsInitialised As Boolean = False

    Sub Initialise()
      If _IsInitialised Then Exit Sub
      Dim res = Reflection.Assembly.GetExecutingAssembly.GetManifestResourceNames
      Dim m = res.Where(Function(r) r.EndsWith("analyserlist.csv"))
      Dim the_file = Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(m(0))
      If the_file Is Nothing Then Exit Sub
      Using CSV As New Microsoft.VisualBasic.FileIO.TextFieldParser(the_file) With {.TrimWhiteSpace = True, .Delimiters = {","}, .TextFieldType = FileIO.FieldType.Delimited}
        CSV.CommentTokens = {"//"}
        While CSV.EndOfData = False

          Dim fields = CSV.ReadFields
          If fields.Count < 5 Then Continue While
          Dim indexOfFormatSTring = 0
          If (Integer.TryParse(fields(2), indexOfFormatSTring) = False) OrElse (indexOfFormatSTring < 0) Then Continue While
          Dim sfd As New SFD_Diag(fields(0), fields(1), indexOfFormatSTring, fields(3), fields.Skip(4).ToArray)
          _Analysis.Add(sfd)
        End While
      End Using
      _IsInitialised = True

    End Sub
    Dim _ToStringAnalysers As New Dictionary(Of String, Func(Of CancellationToken, String, Integer, IFormatProvider, IEnumerable(Of Object), OutputResult(Of String))) From
      {
        {"System.Int16", AddressOf Analysers.Analyse_Numeric_ToString}, {"System.Int32", AddressOf Analysers.Analyse_Numeric_ToString}, {"System.Int64", AddressOf Analysers.Analyse_Numeric_ToString},
        {"System.UInt16", AddressOf Analysers.Analyse_Numeric_ToString}, {"System.UInt32", AddressOf Analysers.Analyse_Numeric_ToString}, {"System.UInt64", AddressOf Analysers.Analyse_Numeric_ToString},
        {"System.Single", AddressOf Analysers.Analyse_Numeric_ToString}, {"System.Double", AddressOf Analysers.Analyse_Numeric_ToString}, {"System.Decimal", AddressOf Analysers.Analyse_Numeric_ToString},
        {"System.Byte", AddressOf Analysers.Analyse_Numeric_ToString}, {"System.UByte", AddressOf Analysers.Analyse_Numeric_ToString},
        {"System.DateTime", AddressOf Analysers.Analyse_DateTime_ToString}, {"System.TimeSpan", AddressOf Analysers.Analyse_TimeSpan_ToString},
        {"System.DateTimeOffset", AddressOf Analysers.Analyse_DateTimeOffset_ToString}, {"System.Enum", AddressOf Analysers.Analyse_Enum_ToString}
    }


    Public ReadOnly Property ToStringAnalysers As IDictionary(Of String, Func(Of CancellationToken, String, Integer, IFormatProvider, IEnumerable(Of Object), OutputResult(Of String)))
      Get
        Return _ToStringAnalysers
      End Get
    End Property

    Public ReadOnly Property Analysis() As IEnumerable(Of SFD_Diag)
      Get
        Return _Analysis
      End Get
    End Property

    '    Public Enum Lang
    '      VB = 0
    '      CS = 1
    '    End Enum

    '    Private _LangAnalysers As New Dictionary(Of Lang, Concurrent.ConcurrentDictionary(Of String, Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object)))
    ')
    '    Public Sub AddLanguageAnalysers(l As Lang, cd As Concurrent.ConcurrentDictionary(Of String, Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object))))
    '      If _LangAnalysers.ContainsKey(l) Then Exit Sub
    '      _LangAnalysers.Add(l, cd)
    '    End Sub

    '    Private Function GetLangAnalyser(l As Lang, a As String) As  Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object))
    '      Dim act As  Action(Of MemberAccessExpressionSyntax, SemanticModel, Action(Of Diagnostic), CancellationToken, Integer, IEnumerable(Of Object)) = nothing
    '      Dim r  = _LangAnalysers(l).TryGetValue(a,act )
    '      Return If(r,act,nothing)
    '    End Function



#Region "Coomonly used Characters"
    Public Const Opening_Brace As Char = "{"c
    Public Const Closing_Brace As Char = "}"c
    Public Const _SPACE_ As Char = " "c
    Public Const _COMMA_ As Char = ","c
    Public Const _COLON_ As Char = ":"c
    Public Const _MINUS_ As Char = "-"c
    Public Const _QUOTE_ As Char = """"c
#End Region

    Public Const _LIMIT_ As Integer = 1000000  ' This limit is found inside the .net implementation of String.Format.
    Public Const ExitOnFirst = False

    Public Function LiteralString(pc As IParsedChar, q As Char) As OutputResult(Of Boolean)
      Dim res As New OutputResult(Of Boolean)
      If pc Is Nothing Then Return res.AddError( New _Internal.Warning(New ArgumentNullException("pc").ToString))
      Dim curr = pc.Next
      While curr.IsEoT AndAlso res.Output = False
        If curr.Value = q Then res.Output = True : Exit While
        curr = curr.Next
      End While
      If Not res.Output Then res.AddError(Errors.UnexpectedlyReachedEndOfText.Default)
      Return res.LastParse(curr)
    End Function


    Public Function DeString(s As String) As String
      ' If a string is included in double qoutes (") remove the match pair.
      If s Is Nothing Then Return ""
      If (s.Length > 0) AndAlso (s.Last = _QUOTE_) Then s = s.Substring(0, s.Length - 1)
      If (s.Length > 0) AndAlso (s.First = _QUOTE_) Then s = s.Substring(1)
      Return s
    End Function




    Public Function ParseValue(pc As IParsedChar, ct As CancellationToken, Limit As Integer, ByRef ParsingIsInAnErrorState As Boolean) As OutputResult(Of Integer)

      Dim _res_ As New OutputResult(Of Integer)
      If pc Is Nothing Then Return _res_
      Do
        If ct.IsCancellationRequested Then Exit Do
        If Not ParsingIsInAnErrorState Then _res_.Output = (10 * _res_.Output) + DigitValue(pc.Value)
        pc = pc.Next
        If pc.IsEoT Then Exit Do
        If Not ParsingIsInAnErrorState AndAlso _res_.Output >= Limit Then ParsingIsInAnErrorState = True
      Loop While IsDigit(pc)
      Return _res_.LastParse(pc)
    End Function

    Public Sub ConsumeSpaces(ByRef pc As IParsedChar, ct As CancellationToken)
      ' Consume spaces
      While (pc IsNot Nothing) AndAlso (Not ct.IsCancellationRequested) AndAlso (pc.Value = _SPACE_)
        pc = pc.Next
      End While
    End Sub
    <Extension>
    Public Function SkipSpaces(pc As IParsedChar, ct As CancellationToken) As IParsedChar
      ' Consume spaces
      While (pc IsNot Nothing) AndAlso (Not ct.IsCancellationRequested) AndAlso (pc.Value = _SPACE_)
        pc = pc.Next
      End While
      Return pc
    End Function

    'Private Function IsDigit(c As ParsedChar) As Boolean
    '  Return ("0"c <= c.Value) AndAlso (c.Value <= "9"c)
    'End Function

    Private Function DigitValue(c As Char) As Integer
      Select Case c
        Case "0"c : Return 0
        Case "1"c : Return 1
        Case "2"c : Return 2
        Case "3"c : Return 3
        Case "4"c : Return 4
        Case "5"c : Return 5
        Case "6"c : Return 6
        Case "7"c : Return 7
        Case "8"c : Return 8
        Case "9"c : Return 9
        Case Else
          Return 0
      End Select
    End Function

  End Module
End Namespace



