Imports System.Threading
Imports AdamSpeight2008.StringFormatDiagnostics.Results

Namespace Global.AdamSpeight2008.StringFormatDiagnostics.Analysers
  Public Module Analysers
    Private _Analysis As New List(Of SFD_Diag)
    Private _IsInitialised As Boolean = False

    'Public Sub Initialise()
    '  If _IsInitialised Then Exit Sub
    '        Dim exe = Reflection.Assembly.GetExecutingAssembly()
    '        'For Each e In exe.GetManifestResourceNames
    '        '    Debug.WriteLine(e)
    '        'Next
    '        Dim the_file = AssemblyMetadat  exe.GetManifestResourceStream("Analysers.analyserlist.csv")
    '        If the_file Is Nothing Then Exit Sub
    '        Using CSV As New Microsoft.VisualBasic.FileIO.TextFieldParser(the_file) With {.TrimWhiteSpace = True, .Delimiters = {","}, .TextFieldType = FileIO.FieldType.Delimited}
    '    CSV.CommentTokens = {"//"}
    '    While CSV.EndOfData = False

    '      Dim fields = CSV.ReadFields
    '      If fields.Count < 5 Then Continue While
    '      Dim indexOfFormatSTring = 0
    '      If (Integer.TryParse(fields(2), indexOfFormatSTring) = False) OrElse (indexOfFormatSTring < 0) Then Continue While
    '      Dim sfd As New SFD_Diag(fields(0), fields(1), indexOfFormatSTring, fields(3), fields.Skip(4).ToArray)
    '      _Analysis.Add(sfd)
    '    End While
    '  End Using
    '  _IsInitialised = True

    'End Sub

    Public ReadOnly Property Analysis() As IEnumerable(Of SFD_Diag)
      Get
        Return _Analysis
      End Get
    End Property

    Dim _ToStringAnalysers As New Dictionary(Of String,
                Func(Of CancellationToken,
                        String,
                        Integer,
                        IFormatProvider,
            IEnumerable(Of Object), Base_Result)) From
              {
                {"System.Int16", AddressOf Analyse_Numeric_ToString},
                {"System.Int32", AddressOf Analyse_Numeric_ToString},
                {"System.Int64", AddressOf Analyse_Numeric_ToString},
                {"System.UInt16", AddressOf Analyse_Numeric_ToString},
                {"System.UInt32", AddressOf Analyse_Numeric_ToString},
                {"System.UInt64", AddressOf Analyse_Numeric_ToString},
                {"System.Single", AddressOf Analyse_Numeric_ToString},
                {"System.Double", AddressOf Analyse_Numeric_ToString},
                {"System.Decimal", AddressOf Analyse_Numeric_ToString},
                {"System.Byte", AddressOf Analyse_Numeric_ToString},
                {"System.UByte", AddressOf Analyse_Numeric_ToString},
                {"System.DateTime", AddressOf Analyse_DateTime_ToString},
                {"System.TimeSpan", AddressOf Analyse_TimeSpan_ToString},
                {"System.DateTimeOffset", AddressOf Analyse_DateTimeOffset_ToString},
                {"System.Enum", AddressOf Analyse_Enum_ToString}
            }


    Public ReadOnly Property ToStringAnalysers As IDictionary(Of String, Func(Of CancellationToken, String, Integer, IFormatProvider, IEnumerable(Of Object), Base_Result))
      Get
        Return _ToStringAnalysers
      End Get
    End Property

  End Module

End Namespace