Imports SFD.Analysis.CS

Namespace Global.SFD.Analysis.CS

  Public Class Checkers_CS

    Dim _DI_ As DiagnosticInfo
    Dim _AS_ As IEnumerable(Of SFD.Analysis.IDiagnosticAnalyser)
    Public ReadOnly Property _DictOfAnalysers As New Dictionary(Of String, IChecker)


    Public Sub New(DI As DiagnosticInfo, A As IEnumerable(Of SFD.Analysis.IDiagnosticAnalyser))
      _AS_ = A
      _DI_ = DI
      If _DictOfAnalysers.Count <> 0 Then Exit Sub
      _DictOfAnalysers = New Dictionary(Of String, IChecker) From
        {{"Num", New Checker(_AS_(0), _DI_)}, {"Date", New Checker(_AS_(1), _DI_)},
         {"TS", New Checker(_AS_(2), _DI_)}, {"DateOff", New Checker(_AS_(3), _DI_)},
         {"Enum", New Checker(_AS_(4), _DI_)}, {"SF", New Checker(_AS_(5), _DI_)}}
    End Sub

  End Class



End Namespace