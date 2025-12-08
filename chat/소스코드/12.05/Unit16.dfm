object Form16: TForm16
  Left = 0
  Top = 0
  Caption = 'Form16'
  ClientHeight = 329
  ClientWidth = 232
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 68
    Height = 21
    Caption = #52828#44396' '#52628#44032
    Color = clBackground
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 232
    Height = 329
    Align = alClient
    TabOrder = 1
    ExplicitLeft = -8
    ExplicitTop = -8
  end
  object Edit1: TEdit
    Left = 0
    Top = 72
    Width = 232
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TextHint = #52828#44396' ID'
  end
  object Button1: TButton
    Left = 68
    Top = 280
    Width = 75
    Height = 25
    Caption = #54869#51064
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 149
    Top = 280
    Width = 75
    Height = 25
    Caption = #45208#44032#44592
    TabOrder = 3
    OnClick = Button2Click
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server='
      'Database='
      'User_Name='
      'Password='
      'CharacterSet=utf8mb4'
      'DriverID=MySQL')
    Connected = True
    LoginPrompt = False
    Left = 64
    Top = 168
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from user')
    Left = 148
    Top = 173
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    VendorLib = '\libmysql.dll'
    Left = 116
    Top = 221
  end
end

