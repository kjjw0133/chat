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
    Top = 63
    Width = 450
    Height = 218
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
  object ScrollBox2: TScrollBox
    Left = 0
    Top = 360
    Width = 450
    Height = 240
    BorderStyle = bsNone
    Color = clWhite
    ParentColor = False
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 0
    Top = 287
    Width = 450
    Height = 73
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 4
    object Label1: TLabel
      Left = 20
      Top = 12
      Width = 119
      Height = 30
      Caption = #48372#45240' '#50836#52397' '#49688
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Malgun Gothic'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object receiverCountLabel: TLabel
      Left = 20
      Top = 48
      Width = 47
      Height = 15
      Caption = #50836#52397' 0'#44148
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Malgun Gothic'
      Font.Style = []
      ParentFont = False
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
'Server=localhost'
      'Database= '
      'User_Name='
      'Password='
      'CharacterSet=utf8mb4'
      'DriverID=MySQL')

    Connected = True
    LoginPrompt = False
    Left = 248
    Top = 184
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
 DriverID = 'MySQL'
    VendorLib = 'libmysql.dll'
    Left = 376
    Top = 192
  end
end
