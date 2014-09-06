Public Class OutputResult(Of T)
  Public Property Output As T
  Private _Errors_ As New List(Of IssueReport)
  Private _valid_ As Boolean = True

  Public Property LastParse As ParsedChar

  Public ReadOnly Property IsValid() As Boolean
    Get
      Return _valid_
    End Get
  End Property
  
  Public ReadOnly Property Errors As IReadOnlyCollection(Of IssueReport)
  Get
      Return _Errors_ 
  End Get
  End Property

  Public Sub AddError(err As IssueReport)
    _valid_ = False
    _Errors_.Add(err)
  End Sub

  Public Function IncludeErrorsFrom(Of Tx)(o As OutputResult(Of Tx)) As OutputResult(Of T)
    _Errors_.AddRange(o._Errors_)
    Return Me
  End Function

End Class