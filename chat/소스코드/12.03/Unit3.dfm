object Form3: TForm3
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Sign Up'
  ClientHeight = 650
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
  object Label5: TLabel
    Left = 157
    Top = 60
    Width = 140
    Height = 47
    Caption = #54924#50896#44032#51077
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -35
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 75
    Top = 150
    Width = 50
    Height = 15
    Caption = #49324#50857#51088' ID'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7763574
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 75
    Top = 230
    Width = 48
    Height = 15
    Caption = #48708#48128#48264#54840
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7763574
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 75
    Top = 310
    Width = 36
    Height = 15
    Caption = #51060#47700#51068
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7763574
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 75
    Top = 390
    Width = 24
    Height = 15
    Caption = #51060#47492
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
    TextHint = #50500#51060#46356
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
    PasswordChar = #9679
    TabOrder = 1
    TextHint = #48708#48128#48264#54840
  end
  object Edit4: TEdit
    Left = 75
    Top = 330
    Width = 150
    Height = 29
    Color = 16250871
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    TextHint = 'email'
  end
  object ComboBox1: TComboBox
    Left = 235
    Top = 330
    Width = 140
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = '@naver.com'
    OnChange = ComboBox1Change
    Items.Strings = (
      '@naver.com'
      '@gmail.com'
      '@daum.net'
      '@nate.com')
  end
  object Edit3: TEdit
    Left = 75
    Top = 410
    Width = 300
    Height = 29
    Color = 16250871
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    TextHint = #51060#47492
  end
  object Button1: TButton
    Left = 75
    Top = 490
    Width = 300
    Height = 50
    Caption = #44032#51077#54616#44592
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = Button1Click
  end
  object FDQueryMembers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from chat')
    Left = 638
    Top = 408
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
    Left = 560
    Top = 304
  end
end
