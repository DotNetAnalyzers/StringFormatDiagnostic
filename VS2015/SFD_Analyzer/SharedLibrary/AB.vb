Namespace Global.SFD.Analysis

  Namespace VB
  Public Structure AB
    Public ReadOnly Property A As IChecker
    Public ReadOnly Property B As SFD_Diag
    Public Sub New(A As IChecker, B As SFD_Diag)
      _A = A
      _B = B
    End Sub
  End Structure
    End Namespace

  Namespace CS
  Public Structure AB
    Public ReadOnly Property A As IChecker
    Public ReadOnly Property B As SFD_Diag
    Public Sub New(A As IChecker, B As SFD_Diag)
      _A = A
      _B = B
    End Sub
  End Structure
    End Namespace

End Namespace