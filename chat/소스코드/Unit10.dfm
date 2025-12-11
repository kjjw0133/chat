object Form10: TForm10
  Left = 0
  Top = 0
  Width = 534
  Height = 470
  AutoScroll = True
  Caption = 'Form10'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 0
    Top = 80
    Width = 79
    Height = 32
    Alignment = taCenter
    Caption = #45236' '#51221#48372
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 200
    Top = 139
    Width = 24
    Height = 15
    Caption = #51060#47492
  end
  object Label4: TLabel
    Left = 280
    Top = 229
    Width = 51
    Height = 15
    Caption = #50976#51200' '#48264#54840
    Visible = False
  end
  object Label3: TLabel
    Left = 200
    Top = 168
    Width = 36
    Height = 15
    Caption = #50500#51060#46356
  end
  object Label5: TLabel
    Left = 200
    Top = 196
    Width = 36
    Height = 15
    Caption = #51060#47700#51068
  end
  object Label6: TLabel
    Left = 280
    Top = 139
    Width = 24
    Height = 15
    Caption = #51060#47492
  end
  object Label7: TLabel
    Left = 280
    Top = 168
    Width = 36
    Height = 15
    Caption = #50500#51060#46356
  end
  object Label8: TLabel
    Left = 200
    Top = 229
    Width = 51
    Height = 15
    Caption = #50976#51200' '#48264#54840
    Visible = False
  end
  object Label9: TLabel
    Left = 280
    Top = 196
    Width = 36
    Height = 15
    Caption = #51060#47700#51068
  end
  object Button1: TButton
    Left = 200
    Top = 280
    Width = 115
    Height = 25
    Caption = #48708#48128#48264#54840' '#51116#49444#51221
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 416
    Top = 24
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
    Left = 88
    Top = 200
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=localhost'
      'Database=chating app'
      'User_Name=root'
      'Password=1234'
      'CharacterSet=utf8mb4'
      'DriverID=MySQL')
    Left = 76
    Top = 293
  end
end
