Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces

Namespace Global.AdamSpeight2008.StringFormatDiagnostic

  Public Class OutputResult
    Friend _Errors_ As New List(Of IReportIssue)
    Friend _valid_ As Boolean = True
    Friend _Last As IParsedChar

    Public Sub New()

    End Sub

    '    Public Property LastParse As IParsedChar

    Public ReadOnly Property Last() As IParsedChar
      Get
        Return _Last
      End Get
    End Property

    Public ReadOnly Property IsValid() As Boolean
      Get
        Return _valid_
      End Get
    End Property

    Public ReadOnly Property Errors As IReadOnlyCollection(Of IReportIssue)
      Get
        Return _Errors_
      End Get
    End Property

    Public Function LastParse(pc As IParsedChar) As OutputResult
      _Last = pc
      Return Me
    End Function

    Public Function AddError(err As IReportIssue) As OutputResult
      If TypeOf err IsNot IssueReports.Information Then _valid_ = False
      _Errors_.Add(err)
      Return Me
    End Function

    Public Function IncludeErrorsFrom(o As OutputResult) As OutputResult
      If o Is Nothing Then Return Me
      _Errors_.AddRange(o._Errors_)
      Return Me
    End Function
    Public Overrides Function ToString() As String
      Return If(_valid_, "(•)", "( )")
    End Function

  End Class

  Public Class OutputResult(Of T)
    Inherits OutputResult

    Public Property Output As T
    Public Shadows Function LastParse(pc As IParsedChar) As OutputResult(Of T)
      _Last = pc
      Return Me
    End Function
    Public Shadows Function AddError(err As IReportIssue ) As OutputResult(Of T)
      If TypeOf err IsNot IssueReports.Information Then _valid_ = False
      _Errors_.Add(err)
      Return Me
    End Function
    Public Shadows Function IncludeErrorsFrom(Of tx)(o As OutputResult(Of tx)) As OutputResult(Of T)
      If o Is Nothing Then Return Me
      _Errors_.AddRange(o._Errors_)
      Return Me
    End Function
    Public Sub New()
      MyBase.New
    End Sub

    Public Sub New(Output As T)
      Me.Output = Output
    End Sub

    'Public Function IncludeErrorsFrom(Of Tx)(o As OutputResult(Of Tx)) As OutputResult(Of T)
    '  If o Is Nothing Then Return Me
    '  _Errors_.AddRange(o._Errors_)
    '  Return Me
    'End Function

    Public Overrides Function ToString() As String
      Return MyBase.ToString() & "  = " & Output.ToString
    End Function

  End Class

End Namespace