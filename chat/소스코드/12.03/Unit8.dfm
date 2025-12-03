object Form8: TForm8
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Create Room'
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
  object Label3: TLabel
    Left = 125
    Top = 60
    Width = 153
    Height = 40
    Caption = #52292#54021#48169' '#49373#49457
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -29
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 75
    Top = 150
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
  object Label2: TLabel
    Left = 75
    Top = 230
    Width = 36
    Height = 15
    Caption = #51064#50896#49688
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7763574
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Visible = False
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
  object Edit1: TEdit
    Left = 75
    Top = 170
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
  object Edit2: TEdit
    Left = 75
    Top = 250
    Width = 300
    Height = 29
    Color = 16250871
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    Text = '1'
    Visible = False
  end
  object RadioButton1: TRadioButton
    Left = 100
    Top = 320
    Width = 113
    Height = 17
    Caption = #48708#44277#44060' '#52292#54021
    TabOrder = 2
  end
  object RadioButton2: TRadioButton
    Left = 240
    Top = 320
    Width = 113
    Height = 17
    Caption = #44277#44060' '#52292#54021
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object Button1: TButton
    Left = 75
    Top = 380
    Width = 300
    Height = 50
    Caption = #52292#54021#48169' '#49373#49457
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = Button1Click
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from chat')
    Left = 310
    Top = 368
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
