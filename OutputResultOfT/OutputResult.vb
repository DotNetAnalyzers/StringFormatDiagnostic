Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces

Namespace Global.AdamSpeight2008.StringFormatDiagnostic

  Public Class OutputResult(Of T)
    Public Property Output As T
    Private _Errors_ As New List(Of IReportIssue)
    Private _valid_ As Boolean = True
    Private _Last As IParsedChar 
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

    Public Function LastParse(pc As IParsedChar ) As OutputResult(Of T)
      _Last = pc
      Return me
    End Function

    Public Function AddError(err As IReportIssue) As OutputResult(Of T)
      If TypeOf err IsNot IssueReports.Information Then _valid_ = False
      _Errors_.Add(err)
      Return Me
    End Function

    Public Function IncludeErrorsFrom(Of Tx)(o As OutputResult(Of Tx)) As OutputResult(Of T)
      If o Is Nothing Then Return Me
      _Errors_.AddRange(o._Errors_)

      Return Me
    End Function

    Public Overrides Function ToString() As String
      Return If(_valid_, "(•)", "( )  = ") & Output.ToString
    End Function

  End Class

End Namespace