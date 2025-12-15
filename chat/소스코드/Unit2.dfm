object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Login'
  ClientHeight = 600
  ClientWidth = 450
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 1644825
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 17
  object Label5: TLabel
    Left = 168
    Top = 100
    Width = 108
    Height = 47
    Caption = 'LOGIN'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -35
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 75
    Top = 200
    Width = 55
    Height = 17
    Caption = #49324#50857#51088' ID'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7763574
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 75
    Top = 280
    Width = 52
    Height = 17
    Caption = #48708#48128#48264#54840
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7763574
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 160
    Top = 530
    Width = 52
    Height = 17
    Cursor = crHandPoint
    Caption = #54924#50896#44032#51077
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7763574
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    OnClick = Label2Click
  end
  object Label4: TLabel
    Left = 250
    Top = 530
    Width = 82
    Height = 17
    Cursor = crHandPoint
    Caption = #48708#48128#48264#54840' '#52286#44592
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7763574
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    OnClick = Label4Click
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object LabelClose: TLabel
      Left = 410
      Top = 15
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
  object Edit1: TEdit
    Left = 75
    Top = 220
    Width = 300
    Height = 29
    Color = 16250871
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    TextHint = #50500#51060#46356#47484' '#51077#47141#54616#49464#50836
  end
  object Edit2: TEdit
    Left = 75
    Top = 300
    Width = 300
    Height = 29
    Color = 16250871
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    PasswordChar = #9679
    TabOrder = 2
    TextHint = #48708#48128#48264#54840#47484' '#51077#47141#54616#49464#50836
  end
  object Button1: TButton
    Left = 75
    Top = 380
    Width = 300
    Height = 50
    Caption = #47196#44536#51064
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 75
    Top = 440
    Width = 300
    Height = 50
    Caption = #44036#54200' '#47196#44536#51064
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button2Click
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from chat')
    Left = 622
    Top = 352
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    DriverID = 'MySQL'
    VendorLib = 'C:\MYPROGRAM\delphi\mysql\mysql-5.7.33-win32\lib\libmysql.dll'
    Left = 548
    Top = 256
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
    Left = 620
    Top = 472
  end
end
