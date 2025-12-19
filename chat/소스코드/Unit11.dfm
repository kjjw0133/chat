object Form11: TForm11
  Left = 0
  Top = 0
  Caption = 'Form11'
  ClientHeight = 531
  ClientWidth = 759
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 103
    Top = 144
    Width = 38
    Height = 25
    Caption = #51060#47492
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 103
    Top = 165
    Width = 57
    Height = 25
    Caption = #50500#51060#46356
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 248
    Top = 133
    Width = 204
    Height = 15
    Caption = #44228#49549#54616#47140#47732' '#47676#51200' '#48376#51064#51076#51012' '#51064#51613#54616#49464#50836'.'
  end
  object Label4: TLabel
    Left = 40
    Top = 144
    Width = 38
    Height = 25
    Caption = #51060#47492
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 40
    Top = 165
    Width = 57
    Height = 25
    Caption = #50500#51060#46356
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 320
    Top = 48
    Width = 76
    Height = 25
    Caption = #48376#51064#54869#51064
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 248
    Top = 165
    Width = 204
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    PasswordChar = '*'
    ShowHint = True
    TabOrder = 0
    TextHint = #48708#48128#48264#54840' '#51077#47141
  end
  object CheckBox1: TCheckBox
    Left = 248
    Top = 215
    Width = 97
    Height = 17
    Caption = #48708#48128#48264#54840' '#54364#49884
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object Button1: TButton
    Left = 280
    Top = 280
    Width = 75
    Height = 25
    Caption = #48708#48128#48264#54840#52286#44592
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 377
    Top = 280
    Width = 75
    Height = 25
    Caption = #45796#51020
    TabOrder = 3
    OnClick = Button2Click
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from chat')
    Left = 550
    Top = 280
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    DriverID = 'MySQL'
    VendorLib = 'C:\MYPROGRAM\delphi\mysql\mysql-5.7.33-win32\lib\libmysql.dll'
    Left = 619
    Top = 184
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=localhost'
      'Database=chating app'
      'User_Name=root'
      'Password=1234'
      'CharacterSet=utf8mb4'
      'DriverID=MySQL')
    Left = 731
    Top = 432
  end
end
