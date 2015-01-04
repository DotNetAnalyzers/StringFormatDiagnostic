Imports System.Threading
Imports SFD.Analysis
  Imports System.Globalization
Imports System.Runtime.CompilerServices
Imports System.Text
Namespace Global.SFD


  Public Module Analysers
    Private _Analysis As New List(Of SFD_Diag)
    Private _IsInitialised As Boolean = False
    Private _DAnalysers() As IDiagnosticAnalyser = {New Numerics, New DateTime, New DateTimeSpan,
                                                    New DateTimeOffset, New Analysis.Enum, New FormatString}

    Public ReadOnly Property DAnalysers() As IEnumerable(Of IDiagnosticAnalyser)
    Get
        Return _DAnalysers
    End Get
    End Property
    Private myLock As New Object 

    Public Sub Initialise()
      SyncLock myLock
      If _IsInitialised =False Then 
      Try
      Dim the_file = Xml.Linq.XDocument.Load("analyserlist.xml")
      If the_file Is Nothing Then Exit Sub
        Dim FoundAnalyserInfomation = the_file.<Analysers>.<Analyser>
        For Each ThisAnalyserInfo In FoundAnalyserInfomation
          Dim ArgIndex = -1
          If Integer.TryParse(ThisAnalyserInfo.@ArgIndex, ArgIndex) Then
            Dim typename = ThisAnalyserInfo.@TypeName
            Dim Args = ThisAnalyserInfo.<Arg>.Select(Function(x) x.@Type).ToArray
            Dim sfd As New SFD_Diag(ThisAnalyserInfo.@TypeName,
                                    ThisAnalyserInfo.@MethodName,
                                    ArgIndex,
                                    ThisAnalyserInfo.@Use,
                                    Args,
                                    ThisAnalyserInfo.@Coloring)
            _Analysis.Add(SFD)
          End If
        Next
        _IsInitialised = True
          Catch ex As Exception
          Debug.WriteLine(ex)
          End Try
        End if
      End SyncLock


    End Sub

    Public ReadOnly Property Analysis() As IEnumerable(Of SFD_Diag)
      Get
        Return _Analysis
      End Get
    End Property

    Dim _ToStringAnalysers As New Dictionary(Of String,
    IDiagnosticAnalyser) From
            {
              {"System.Int16", _DAnalysers(0)},
              {"System.Int32", _DAnalysers(0)},
              {"System.Int64", _DAnalysers(0)},
              {"System.UInt16", _DAnalysers(0)},
              {"System.UInt32", _DAnalysers(0)},
              {"System.UInt64", _DAnalysers(0)},
              {"System.Single", _DAnalysers(0)},
              {"System.Double", _DAnalysers(0)},
              {"System.Decimal", _DAnalysers(0)},
              {"System.Byte", _DAnalysers(0)},
              {"System.UByte", _DAnalysers(0)},
              {"System.DateTime", _DAnalysers(1)},
              {"System.TimeSpan", _DAnalysers(2)},
              {"System.DateTimeOffset", _DAnalysers(3)},
              {"System.Enum", _DAnalysers(4)}
          }


    Public ReadOnly Property ToStringAnalysers As IDictionary(Of String, IDiagnosticAnalyser)
      Get
        Return _ToStringAnalysers
      End Get
    End Property

  End Module

End Namespace