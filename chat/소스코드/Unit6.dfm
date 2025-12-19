object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Form6'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 248
    Top = 88
    Width = 133
    Height = 25
    Caption = #48708#48128#48264#54840#51116#49444#51221
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 208
    Top = 152
    Width = 48
    Height = 15
    Caption = #48708#48128#48264#54840
  end
  object Label3: TLabel
    Left = 208
    Top = 237
    Width = 75
    Height = 15
    Caption = #48708#48128#48264#54840' '#54869#51064
  end
  object Edit1: TEdit
    Left = 208
    Top = 184
    Width = 209
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TextHint = #48708#48128#48264#54840#47484' '#51077#47141#54616#49464#50836
  end
  object Button1: TButton
    Left = 208
    Top = 320
    Width = 209
    Height = 41
    Caption = #48708#48128#48264#54840' '#48320#44221
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 208
    Top = 269
    Width = 209
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    TextHint = #48708#48128#48264#54840#47484' '#51077#47141#54616#49464#50836
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from chat')
    Left = 494
    Top = 296
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=localhost'
      'Database=chating app'
      'User_Name=root'
      'Password=1234'
      'CharacterSet=utf8mb4'
      'DriverID=MySQL')
    Left = 548
    Top = 213
  end
end
