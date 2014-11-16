Imports AdamSpeight2008.StringFormatDiagnostics.Errors
Imports AdamSpeight2008.StringFormatDiagnostics.Warnings

Namespace Global.AdamSpeight2008.StringFormatDiagnostics

  Public Class SFD_Diag
    Public ReadOnly Property TypeName As String = ""
    Public ReadOnly Property MethodName As String = ""
    Public ReadOnly Property FIndex As Integer
    Public ReadOnly Property Analyser As String = ""
    Public ReadOnly Property ParamTypes As String() = {}

    Public Sub New(TypeName As String, MethodName As String, FIndex As Integer, Analyser As String, ParamTypes As String())
      _TypeName = If(TypeName, "")
      _MethodName = If(MethodName, "")
      _FIndex = FIndex
      _Analyser = If(Analyser, "")
      _ParamTypes = If(ParamTypes, {})
    End Sub
  End Class


  Namespace Results

    Public MustInherit Class Base_Result
      Friend _Errors_ As New List(Of Parse_Error)
      Friend _Warnings_ As New List(Of Parse_Warning)

      Friend Sub New()
      End Sub

      Public Overrides Function ToString() As String
        Return ""
      End Function

      Public ReadOnly Property Errors As IEnumerable(Of Parse_Error)
        Get
          Return _Errors_
        End Get
      End Property

      Public Sub IncludeFrom(Of t As Base_Result)(br As t)
        _Errors_.AddRange(br.Errors)
        _Warnings_.AddRange(br.Warnings)
      End Sub

      Public Sub AddError(e As Parse_Error)
        If e IsNot Nothing Then _Errors_.Add(e)
      End Sub
      Public ReadOnly Property Warnings As IEnumerable(Of Parse_Warning)
        Get
          Return _Warnings_
        End Get
      End Property
      Public Sub AddWarning(e As Parse_Warning)
        If e IsNot Nothing Then _Warnings_.Add(e)
      End Sub

    End Class

    Public Class Result(Of T)
      Inherits Base_Result
      Public Property Value As T

      Public Sub New(Value As T)
        MyBase.New
        _Value = Value
      End Sub

      Public Overrides Function ToString() As String
        Return Value.ToString
      End Function

    End Class

    Public Class ParseResult(Of T)
      Inherits Result(Of T)
      Public ReadOnly Property SR As StringReader
      Public Sub New(sr As StringReader, value As T)
        MyBase.New(value)
        _SR = sr
      End Sub
    End Class

    Public Class Error_Result
      Inherits Base_Result
      Public ReadOnly Property [Error] As Parse_Error
      Public Sub New(e As Parse_Error)
        MyBase.New
        Me._Error = e
      End Sub
    End Class

    Public Class Warning_Result
      Inherits Results.Base_Result
      Public ReadOnly Property Warning As Parse_Warning
      Public Sub New(w As Parse_Warning)
        MyBase.New()
        _Warning = w
      End Sub
    End Class

  End Namespace

  Namespace Errors

    Public Class SpecifierUnknown
      Inherits Parse_ErrorSpan
      Private _MSg As String = ""
      Public Sub New(specifier As String, Start As Integer)
        MyBase.New(New IndexSpan(Start, specifier.Length))
        _MSg = String.Format("Specifier Unknown [{0}] at {1}.", specifier, Start)
      End Sub
      Public Overrides Function ToString() As String
        Return _MSg
      End Function
    End Class

    Public MustInherit Class Parse_Error
      Friend Sub New()

      End Sub
    End Class

    Public MustInherit Class Parse_ErrorSpan
      Inherits Parse_Error
      Public ReadOnly Property Index As IndexSpan
      Friend Sub New(s As IndexSpan)
        MyBase.New
        _Index = s
      End Sub
    End Class

    Public Class MissingArgIndex
      Inherits Parse_ErrorSpan
      Public Sub New(s As IndexSpan)
        MyBase.New(s)
      End Sub

      Public Overrides Function ToString() As String
        Return "Arg Index is missing"
      End Function
    End Class

    Public Class NegativeArgIndex
      Inherits Parse_ErrorSpan
      Public Sub New(s As IndexSpan)
        MyBase.New(s)
      End Sub

      Public Overrides Function ToString() As String
        Return "Arg Index can not be negative"
      End Function
    End Class

    Public MustInherit Class BoundedValue
      Inherits Parse_ErrorSpan

      Friend _limit_ As Integer
      Friend _value_ As Integer
      Friend Sub New(s As IndexSpan, value As Integer, limit As Integer)
        MyBase.New(s)
        _value_ = value
        _limit_ = limit
      End Sub
    End Class

    Public Class ArgIndexBeyondEnd
      Inherits BoundedValue
      Public Sub New(s As IndexSpan, value As Integer, limit As Integer)
        MyBase.New(s, value, limit)
      End Sub

      Public Overrides Function ToString() As String
        Return String.Format("Arg Index is out of range of args supplied. (0 <= {0} < {1})", _value_, _limit_)
      End Function
    End Class

    Public Class ArgIndexBeyondLimit
      Inherits BoundedValue
      Public Sub New(s As IndexSpan, value As Integer, limit As Integer)
        MyBase.New(s, value, limit)
      End Sub

      Public Overrides Function ToString() As String
        Return String.Format("Arg Index is beyond .net limit. (0 <= {0} < {1})", _value_, _limit_)
      End Function
    End Class

    Public Class ValueHasExceededLimit
      Inherits BoundedValue
      Public ReadOnly Property ValueName As String
      Public Sub New(ValueName As String, s As IndexSpan, value As Integer, limit As Integer)
        MyBase.New(s, value, limit)
        _ValueName = ValueName
      End Sub

      Public Overrides Function ToString() As String
        Return String.Format("{2} has exceeded limit. (0 <= {0} < {1})", _value_, _limit_, ValueName)
      End Function
    End Class

    Public Class AligmentValueExceeded
      Inherits BoundedValue
      Public Sub New(s As IndexSpan, value As Integer, limit As Integer)
        MyBase.New(s, value, limit)
      End Sub

      Public Overrides Function ToString() As String
        Return String.Format("Arg Index is out of range of args supplied. (-{1} < {0} < {1})", _value_, _limit_)
      End Function
    End Class

    Public MustInherit Class Parse_ErrorAtIndex
      Inherits Parse_Error
      Public ReadOnly Property Index As Integer
      Friend Sub New(Index As Integer)
        MyBase.New
        _Index = Index
      End Sub
    End Class

    Public Class UnexpectedChar
      Inherits Parse_ErrorAtIndex
      Public ReadOnly Property C As Char

      Public Sub New(Index As Integer, C As Char)
        MyBase.New(Index)
        _C = C
      End Sub
      Public Overrides Function ToString() As String
        Return String.Format("Unexpected Char '{0}'", C)
      End Function
    End Class

    Public Class UnknownSpecifier
      Inherits Parse_ErrorAtIndex
      Public ReadOnly Property C As Char

      Public Sub New(Index As Integer, C As Char)
        MyBase.New(Index)
        _C = C
      End Sub
      Public Overrides Function ToString() As String
        Return String.Format("Unknown specifier '{0}'", C)
      End Function
    End Class

    Public Class UnexpectedEoT
      Inherits Parse_Error

      Public Sub New()
        MyBase.New()
      End Sub
      Public Shared Function [Default]() As UnexpectedEoT
        Return New UnexpectedEoT
      End Function
      Public Overrides Function ToString() As String
        Return "Unexpectedly reached End Of Text"
      End Function
    End Class

    Public Class NonInteger
      Inherits Parse_ErrorAtIndex
      Public ReadOnly Property Span As Integer

      Public Sub New(sp As IndexSpan)
        MyBase.New(sp.Index)
        _Span = sp.Span
      End Sub

    End Class

  End Namespace

  Namespace Warnings

    Public MustInherit Class Parse_Warning
      Friend Sub New()
        MyBase.New()
      End Sub
    End Class

    Public Class NoArgs
      Inherits Parse_Warning
      Public Sub New()
        MyBase.New()
      End Sub
      Public Overrides Function ToString() As String
        Return "No Parameter Arguments Supplied"
      End Function
    End Class
    Public Class UnusedArg
      Inherits Parse_Warning
      Public ReadOnly Property ArgIndex As Integer
      Public Sub New(Arg As Integer)
        MyBase.New()
        Me._ArgIndex = Arg
      End Sub
      Public Overrides Function ToString() As String
        Return String.Format("Parameter Argument ({0}) Unused", ArgIndex)
      End Function
    End Class
    Public Class NoHoles
      Inherits Parse_Warning
      Public Sub New()
        MyBase.New()
      End Sub
      Public Overrides Function ToString() As String
        Return "No Arg Holes found in text."
      End Function
    End Class

    Public Class TooManySections
      Inherits Parse_Warning
      Public ReadOnly Property AtIndex As Integer
      Public Sub New(AtIndex As Integer)
        _AtIndex = AtIndex
      End Sub
    End Class

    Public MustInherit Class WarningAtIndex
      Inherits Parse_Warning
      Public ReadOnly Property AtIndex As Integer

      Friend Sub New(index As Integer)
        _AtIndex = index
      End Sub
    End Class

    Public Class IgnoredChar
      Inherits WarningAtIndex
      Public ReadOnly Property C As Char

      Public Sub New(Index As Integer, C As Char)
        MyBase.New(Index)
        _C = C
      End Sub
      Public Overrides Function ToString() As String
        Return String.Format("Ignored Char '{0}'", C)
      End Function
    End Class

  End Namespace

End Namespace
