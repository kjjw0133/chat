object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'ChatApp'
  ClientHeight = 700
  ClientWidth = 436
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
    Width = 436
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    OnMouseDown = PanelHeaderMouseDown
    object LabelTitle: TLabel
      Left = 78
      Top = 10
      Width = 86
      Height = 30
      Caption = 'ChatApp'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object SpeedButtonMenu: TSpeedButton
      Left = 336
      Top = 10
      Width = 30
      Height = 30
      Caption = #9881
      Flat = True
    end
    object SpeedButtonSearch: TSpeedButton
      Left = 300
      Top = 15
      Width = 30
      Height = 30
      Caption = #62733
      Flat = True
    end
    object LabelClose: TLabel
      Left = 390
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
  end
  object PanelActions: TPanel
    Left = 0
    Top = 60
    Width = 436
    Height = 70
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object Edit2: TEdit
      Left = 56
      Top = 13
      Width = 120
      Height = 38
      Color = 16250871
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      TextHint = #48169' '#48264#54840
    end
    object Edit1: TEdit
      Left = 182
      Top = 14
      Width = 120
      Height = 38
      Color = 16250871
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      PasswordChar = #9679
      TabOrder = 1
      TextHint = #48708#48128#48264#54840
    end
    object Button6: TButton
      Left = 308
      Top = 13
      Width = 55
      Height = 40
      Caption = #51077#51109
      TabOrder = 2
      OnClick = Button6Click
    end
    object Button3: TButton
      Left = 367
      Top = 13
      Width = 55
      Height = 40
      Caption = #49373#49457
      TabOrder = 3
      OnClick = Button3Click
    end
  end
  object ScrollBox1: TScrollBox
    Left = 78
    Top = 134
    Width = 363
    Height = 510
    Color = clWhite
    ParentColor = False
    TabOrder = 2
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 640
    Width = 436
    Height = 60
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    object Button4: TButton
      Left = 32
      Top = 10
      Width = 120
      Height = 40
      Caption = #47196#44536#51064
      TabOrder = 0
      OnClick = Button4Click
    end
    object Button1: TButton
      Left = 158
      Top = 10
      Width = 120
      Height = 40
      Caption = #54924#50896#44032#51077
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 284
      Top = 10
      Width = 120
      Height = 40
      Caption = #47560#51060#54168#51060#51648
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 134
    Width = 81
    Height = 510
    Color = clSilver
    ParentBackground = False
    TabOrder = 4
    object SpeedButton1: TSpeedButton
      Left = 5
      Top = 24
      Width = 67
      Height = 49
      Caption = #52828#44396' '#52285
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 5
      Top = 96
      Width = 67
      Height = 49
      Caption = #47700#51064' '#52292#54021' '#52285
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object SpeedButton3: TSpeedButton
      Left = 5
      Top = 168
      Width = 67
      Height = 49
      Caption = #45908#48372#44592
      OnClick = SpeedButton3Click
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
    Left = 88
    Top = 296
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from user')
    Left = 224
    Top = 408
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 326
    Top = 320
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
     DriverID = 'MySQL'
    VendorLib = 'libmysql.dll'
    Left = 112
    Top = 400
  end
end
