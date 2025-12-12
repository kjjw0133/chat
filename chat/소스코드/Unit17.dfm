object Form17: TForm17
  Left = 0
  Top = 0
  Caption = #52828#44396' '#50836#52397
  ClientHeight = 600
  ClientWidth = 450
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object LabelTitle: TLabel
      Left = 20
      Top = 12
      Width = 91
      Height = 30
      Caption = #52828#44396' '#50836#52397
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Malgun Gothic'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LabelClose: TLabel
      Left = 410
      Top = 8
      Width = 16
      Height = 32
      Cursor = crHandPoint
      Caption = #215
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnClick = LabelCloseClick
    end
    object requestCountLabel: TLabel
      Left = 20
      Top = 42
      Width = 75
      Height = 15
      Caption = #52828#44396' '#50836#52397' 0'#44148
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Malgun Gothic'
      Font.Style = []
      ParentFont = False
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 79
    Width = 450
    Height = 461
    BorderStyle = bsNone
    Color = clWhite
    ParentColor = False
    TabOrder = 1
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 540
    Width = 450
    Height = 60
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server='
      'Database='
      'User_Name='
      'Password='
      'CharacterSet='
      'DriverID=MySQL')
    Connected = True
    LoginPrompt = False
    Left = 88
    Top = 296
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    VendorLib = '\libmysql.dll'
    Left = 208
    Top = 352
  end
end

