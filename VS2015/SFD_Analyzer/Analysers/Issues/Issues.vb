Imports Analysers
Imports SFD.StringFormat
Namespace Global.SFD.Analysis
Public Module Issues

  Public Enum IssueKinds
    Info
    [Error]
    Warning
    Hidden
  End Enum

  Public MustInherit Class Base_Issue
  
    Public ReadOnly Property Span As SpanKind
    Public ReadOnly Property Kind As Issues.IssueKinds 
    Public ReadOnly Property ID As String
    Public ReadOnly Property Text As String

    Friend Sub New(Span As SpanKind, Kind As Issues.IssueKinds, ID As String,
                   Text As String)
      _Span = Span
        _Kind = Kind
        _ID = If(ID,"")
        _Text = If(Text, "")
      End Sub


    End Class

  Public Class Unused_Arg
    Inherits Base_Issue
    Public ReadOnly Property Value As Integer
    Public Sub New(span As SpanKind, value As Integer)
        MyBase.New(span, IssueKinds.Info, "SFD_01","Argument is unused in format string." )
        _Value = value
    End Sub
  End Class

  Public Class ArgIndex_Negative
    Inherits Base_Issue
    Public Sub New(span As SpanKind)
      MyBase.New(span, IssueKinds.Error, "SFD_02","ArgIndex can not be negative.")
    End Sub
  End Class

  Public Class ArgIndex_DotNetLimit_Exceeded
    Inherits Base_Issue
    Public Sub New(span As SpanKind)
      MyBase.New(span, IssueKinds.Error, "SFD_03","ArgIndex exceeds .net limit.")
    End Sub
  End Class

  Public Class ArgIndex_Exceeded
    Inherits Base_Issue

    Public ReadOnly Property Value As Integer
    Public ReadOnly Property Limit As Integer

    Public Sub New(Value As Integer, Limit As Integer, span As SpanKind)
        MyBase.New(span, IssueKinds.Error, "SFD_04",String.Format("ArgIndex ({0}) not in range (0 to {1})",Value,Limit-1))
        _Value = Value
      _Limit = Limit
    End Sub
  End Class
    Public Class NoArgs
    Inherits Base_Issue
    Public Sub New(span As SpanKind)
      MyBase.New(span,IssueKinds.Info, "SFD_05","No Arguments supplied. (Is this correct?)")
    End Sub
  End Class
    Public Class NoHoles
      Inherits Base_Issue
      Public Sub New(span As SpanKind)
        MyBase.New(span, IssueKinds.Info, "SFD_06", "No ArgHoles found. (Is this correct?)")
      End Sub
    End Class
  Public Class Err_UC
    Inherits Base_Issue
    Private Value As Char
    Public Sub New(span As SpanKind, value As char)
      MyBase.New(span, IssueKinds.Error, "SFD_07",String.Format("Unexpected character {0}",value))
      Me.Value = value
    End Sub
  End Class

    Public Class Err_NoArgIndex
      Inherits Base_Issue
      Public Sub New(span As SpanKind)
        MyBase.New(span, IssueKinds.Error, "SFD_08", "No ArgIndex")
      End Sub
    End Class

    Public Class Err_NoAlign
      Inherits Base_Issue
      Public Sub New(span As SpanKind)
        MyBase.New(span, IssueKinds.Error, "SFD_09","No ArgAlign")
      End Sub
    End Class

    Public Class ArgAlign_DotNetLimit_Exceeded
    Inherits Base_Issue
    Public Sub New(span As SpanKind)
        MyBase.New(span, IssueKinds.Error, "SFD_10", "ArgAlign exceeds .net limit. ")
      End Sub
  End Class

  Public Class UnknownSpecifier
    Inherits Base_Issue
    Public ReadOnly Property Value As Char
    Public Sub New(span As SpanKind, Value As Char)
        MyBase.New(span, IssueKinds.Error, "SFD_11",String.Format("Unknown Specifier ({0})",Value))
        _Value = Value
    End Sub
  End Class
    Friend Class UnexpectedEoT
      Inherits Base_Issue

      Public Sub New(span As SpanKind)
        MyBase.New(span, IssueKinds.Error, "SFD_12", "Unexpectedly reached end of text")
      End Sub
    End Class
    Friend Class TooManySections
    Inherits Base_Issue

      Public Sub New(span As SpanKind)
        MyBase.New(span, IssueKinds.Error, "SFD_13", "Too Many Sections")
      End Sub
    End Class
    Public Class SpecifierUnknown
      Inherits Base_Issue
      Public ReadOnly Property Value As String
      Public Sub New(span As SpanKind, Value As String)
        MyBase.New(span, IssueKinds.Error, "SFD_14", String.Format("Specifier Unknown ({0})", Value))
        _Value = Value
      End Sub
    End Class

    Friend Class IgnoredChar
    Inherits Base_Issue

    Private value As Char

      Public Sub New(span As SpanKind, value As Char)
        MyBase.New(span, IssueKinds.Info, "SFD_15", "Ignored Character")
        Me.value = value
      End Sub
    End Class

    Friend Class ValueHasExceededLimit
      Inherits Base_Issue

      Private indexSpan As Span
      Private Value As Integer
      Private Limit As Integer

      Public Sub New(span As SpanKind, value As Integer, Limit As Integer, name As String)
        MyBase.New(span, IssueKinds.Error, "SFD_16", String.Format("{2} ({0}) has exceeded limit ({1})", value, Limit, name))
        Me.indexSpan = indexSpan
        Me.Value = value
        Me.Limit = Limit
      End Sub
    End Class

    Friend Class NotAnInteger
      Inherits Base_Issue

      Public Sub New(argAlign As SpanKind)
        MyBase.New(argAlign, IssueKinds.Error, "SFD_17", "Not an Integer Value")
      End Sub
    End Class

    Public Class Missing_LB
      Inherits Base_Issue
      Public Sub New(span As SpanKind)
        MyBase.New(span, IssueKinds.Error, "SFD_18", "Are you missing an opening brace?")
      End Sub
    End Class
    Public Class Missing_RB
      Inherits Base_Issue
      Public Sub New(span As SpanKind)
        MyBase.New(span, IssueKinds.Error, "SFD_19", "Are you missing a closing brace?")
      End Sub
    End Class
  End Module

End Namespace