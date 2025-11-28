object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ChatRoom'
  ClientHeight = 700
  ClientWidth = 420
  Color = 12174809
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 420
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    Color = 16448243
    ParentBackground = False
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 10
      Top = 15
      Width = 40
      Height = 30
      Caption = #8592
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton1Click
    end
    object Label1: TLabel
      Left = 60
      Top = 18
      Width = 52
      Height = 21
      Caption = #48169' '#51060#47492
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Label2: TLabel
      Left = 180
      Top = 20
      Width = 7
      Height = 17
      Caption = '3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 7763574
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Transparent = False
    end
    object SpeedButton3: TSpeedButton
      Left = 370
      Top = 15
      Width = 40
      Height = 30
      Caption = #8942
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      PopupMenu = PopupMenu1
      OnClick = SpeedButton3Click
    end
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 60
    Width = 420
    Height = 580
    Align = alClient
    Alignment = taCenter
    Color = 12174809
    Font.Charset = HANGEUL_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    OnChange = RichEdit1Change
  end
  object PanelInput: TPanel
    Left = 0
    Top = 640
    Width = 420
    Height = 60
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    object Edit1: TEdit
      Left = 10
      Top = 10
      Width = 330
      Height = 25
      Color = 16250871
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      TextHint = #47700#49884#51648#47484' '#51077#47141#54616#49464#50836
      OnKeyDown = Edit1KeyDown
    end
    object Button1: TButton
      Left = 350
      Top = 10
      Width = 60
      Height = 40
      Caption = #51204#49569
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from chat')
    Left = 702
    Top = 488
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    DriverID = 'MySQL'
    VendorLib = 'C:\MYPROGRAM\delphi\mysql\mysql-5.7.33-win32\lib\libmysql.dll'
    Left = 718
    Top = 408
  end
  object PopupMenu1: TPopupMenu
    Left = 152
    Top = 144
    object N4: TMenuItem
      Caption = #47560#51060#54168#51060#51648
      OnClick = N4Click
    end
    object N1: TMenuItem
      Caption = #48169' '#51221#48372
      OnClick = N1Click
    end
    object N2: TMenuItem
      Caption = #48169' '#45208#44032#44592
      OnClick = N2Click
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=localhost'
      'Database=chating app'
      'User_Name=root'
      'Password=1234'
      'CharacterSet=utf8mb4'
      'DriverID=MySQL')
    Connected = True
    LoginPrompt = False
    Left = 720
    Top = 312
  end
  object ClientSocket1: TClientSocket
    Active = False
    Address = '127.0.0.1'
    ClientType = ctNonBlocking
    Port = 8080
    OnConnect = ClientSocket1Connect
    OnRead = ClientSocket1Read
    OnError = ClientSocket1Error
    Left = 672
    Top = 64
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 728
    Top = 136
  end
end
