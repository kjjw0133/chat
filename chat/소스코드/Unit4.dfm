object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 512
  ClientWidth = 724
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 208
    Top = 136
    Width = 36
    Height = 15
    Caption = #50500#51060#46356
  end
  object Label2: TLabel
    Left = 208
    Top = 176
    Width = 36
    Height = 15
    Caption = #51060#47700#51068
  end
  object Label3: TLabel
    Left = 256
    Top = 88
    Width = 114
    Height = 25
    Caption = #48708#48128#48264#54840#52286#44592
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Button1: TButton
    Left = 240
    Top = 232
    Width = 121
    Height = 33
    Caption = #48708#48128#48264#54840' '#52286#44592
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 272
    Top = 133
    Width = 121
    Height = 23
    TabOrder = 1
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 272
    Top = 173
    Width = 121
    Height = 23
    TabOrder = 2
    Text = 'Edit1'
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from chat')
    Left = 510
    Top = 352
  end
  object IdMessage1: TIdMessage
    AttachmentEncoding = 'UUE'
    BccList = <>
    CCList = <>
    Encoding = meDefault
    FromList = <
      item
      end>
    Recipients = <>
    ReplyTo = <>
    ConvertPreamble = True
    Left = 152
    Top = 388
  end
  object IdSMTP1: TIdSMTP
    IOHandler = IdSSLIOHandlerSocketOpenSSL1
    Port = 465
    SASLMechanisms = <>
    UseTLS = utUseImplicitTLS
    Left = 256
    Top = 396
  end
  object IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':465'
    MaxLineAction = maException
    Port = 465
    DefaultPort = 0
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 376
    Top = 396
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=localhost'
      'Database=chating app'
      'User_Name=root'
      'Password=1234'
      'CharacterSet=utf8mb4'
      'DriverID=MySQL')
    Left = 616
    Top = 400
  end
end
