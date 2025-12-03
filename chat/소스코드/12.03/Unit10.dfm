object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 264
    Top = 80
    Width = 79
    Height = 32
    Caption = #45236' '#51221#48372
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 184
    Top = 147
    Width = 24
    Height = 15
    Caption = #51060#47492
  end
  object Label4: TLabel
    Left = 209
    Top = 333
    Width = 48
    Height = 15
    Caption = #48708#48128#48264#54840
    Visible = False
  end
  object Label3: TLabel
    Left = 184
    Top = 176
    Width = 36
    Height = 15
    Caption = #50500#51060#46356
  end
  object Label5: TLabel
    Left = 184
    Top = 204
    Width = 36
    Height = 15
    Caption = #51060#47700#51068
  end
  object Label6: TLabel
    Left = 264
    Top = 147
    Width = 24
    Height = 15
    Caption = #51060#47492
  end
  object Label7: TLabel
    Left = 264
    Top = 176
    Width = 36
    Height = 15
    Caption = #50500#51060#46356
  end
  object Label8: TLabel
    Left = 136
    Top = 333
    Width = 48
    Height = 15
    Caption = #48708#48128#48264#54840
    Visible = False
  end
  object Label9: TLabel
    Left = 264
    Top = 204
    Width = 36
    Height = 15
    Caption = #51060#47700#51068
  end
  object Button1: TButton
    Left = 228
    Top = 288
    Width = 115
    Height = 25
    Caption = #48708#48128#48264#54840' '#51116#49444#51221
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 472
    Top = 48
    Width = 75
    Height = 25
    Caption = #45208#44032#44592
    TabOrder = 1
    OnClick = Button2Click
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from chat')
    Left = 480
    Top = 232
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=localhost'
      'Database=chating app'
      'User_Name=root'
      'Password=1234'
      'CharacterSet=utf8mb4'
      'DriverID=MySQL')
    Left = 524
    Top = 173
  end
end
