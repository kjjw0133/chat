object Form14: TForm14
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
  OnDestroy = FormDestroy
  TextHeight = 15
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 436
    Height = 43
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object LabelTitle: TLabel
      Left = 70
      Top = 10
      Width = 42
      Height = 30
      Caption = #52828#44396
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
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
    end
  end
  object PanelActions: TPanel
    Left = 0
    Top = 43
    Width = 436
    Height = 70
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object userCountLabel: TLabel
      Left = 70
      Top = 45
      Width = 45
      Height = 15
      Caption = #52828#44396' 0'#47749
    end
    object Edit2: TEdit
      Left = 70
      Top = 10
      Width = 310
      Height = 29
      Color = 16250871
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      TextHint = #52828#44396' '#44160#49353
      OnChange = Edit2Change
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 113
    Width = 57
    Height = 527
    Align = alLeft
    TabOrder = 2
    object SpeedButton1: TSpeedButton
      Left = 0
      Top = 24
      Width = 57
      Height = 49
      Caption = #52828#44396' '#52285
    end
    object SpeedButton2: TSpeedButton
      Left = 0
      Top = 96
      Width = 57
      Height = 49
      Caption = #47700#51064' '#52292#54021' '#52285
      OnClick = SpeedButton2Click
    end
    object SpeedButton3: TSpeedButton
      Left = 0
      Top = 168
      Width = 57
      Height = 49
      Caption = #45908#48372#44592
      OnClick = SpeedButton3Click
    end
  end
  object lbFriends: TListBox
    Left = 57
    Top = 113
    Width = 379
    Height = 527
    Align = alClient
    BorderStyle = bsNone
    Color = clWhite
    ItemHeight = 15
    TabOrder = 3
    OnDrawItem = lbFriendsDrawItem
    OnMeasureItem = lbFriendsMeasureItem
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
    TabOrder = 4
  end
  object Button1: TButton
    Left = 309
    Top = 12
    Width = 75
    Height = 25
    Caption = #52828#44396' '#52628#44032
    TabOrder = 5
    OnClick = Button1Click
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server='
      'Database= '
      'User_Name='
      'Password='
      'CharacterSet='
      'DriverID=MySQL')
    Connected = True
    LoginPrompt = False
    Left = 176
    Top = 408
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from user')
    Left = 312
    Top = 424
  end
  object Timer1: TTimer
    Left = 350
    Top = 376
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    VendorLib = '\libmysql.dll'
    Left = 208
    Top = 352
  end
end

