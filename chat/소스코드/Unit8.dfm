object Form8: TForm8
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Form8'
  ClientHeight = 500
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
  object Label1: TLabel
    Left = 75
    Top = 166
    Width = 39
    Height = 15
    Caption = #48169' '#51228#47785
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7763574
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
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
  object Label2: TLabel
    Left = 0
    Top = 60
    Width = 127
    Height = 32
    Alignment = taCenter
    Caption = #52292#54021#48169' '#49373#49457
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 75
    Top = 187
    Width = 300
    Height = 29
    Color = 16250871
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TextHint = #48169' '#51228#47785#51012' '#51077#47141#54616#49464#50836
  end
  object Button2: TButton
    Left = 75
    Top = 320
    Width = 110
    Height = 49
    Caption = #51068#48152' '#52292#54021
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 265
    Top = 320
    Width = 110
    Height = 49
    Caption = #50724#54536' '#52292#54021
    TabOrder = 2
    OnClick = Button3Click
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from chat')
    Left = 382
    Top = 256
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    DriverID = 'MySQL'
    VendorLib = 'C:\MYPROGRAM\delphi\mysql\mysql-5.7.33-win32\lib\libmysql.dll'
    Left = 548
    Top = 240
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
    Left = 553
    Top = 376
  end
end
