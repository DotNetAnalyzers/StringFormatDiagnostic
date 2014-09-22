Imports AdamSpeight2008.StringFormatDiagnostic.Interfaces

Namespace Global.AdamSpeight2008.StringFormatDiagnostic

Public Class OutputResult(Of T)
  Public Property Output As T
  Private _Errors_ As New List(Of IReportIssue)
  Private _valid_ As Boolean = True

  Public Property LastParse As IParsedChar

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

  Public Sub AddError(err As IReportIssue)
    If TypeOf err IsNot IssueReports.Information Then _valid_ = False
    _Errors_.Add(err)
  End Sub

  Public Function IncludeErrorsFrom(Of Tx)(o As OutputResult(Of Tx)) As OutputResult(Of T)
      If o Is Nothing then Return Me
   _Errors_.AddRange(o._Errors_)

    Return Me
  End Function

    Public Overrides Function ToString() As String
      Return If(_valid_,"(•)", "( )  = ") & Output.ToString  
    End Function

  End Class

End Namespace