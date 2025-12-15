object Form13: TForm13
  Left = 0
  Top = 0
  Caption = #45824#54868#49345#45824' '#49440#53469
  ClientHeight = 560
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object LabelTitle: TLabel
    Left = 16
    Top = 12
    Width = 100
    Height = 21
    Caption = #45824#54868#49345#45824' '#49440#53469
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object userCountLabel: TLabel
    Left = 16
    Top = 93
    Width = 46
    Height = 13
    Caption = #52828#44396' ??'#47749
  end
  object pnlSearch: TPanel
    Left = 16
    Top = 44
    Width = 332
    Height = 40
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object pbSearchBG: TPaintBox
      Left = 0
      Top = 0
      Width = 332
      Height = 40
      Align = alClient
      OnPaint = pbSearchBGPaint
      ExplicitWidth = 241
    end
    object edtSearch: TEdit
      Left = 36
      Top = 8
      Width = 280
      Height = 24
      Margins.Left = 40
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = edtSearchChange
      OnEnter = edtSearchEnter
      OnExit = edtSearchExit
    end
  end
  object lbFriends: TListBox
    Left = 16
    Top = 120
    Width = 336
    Height = 376
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbFriendsClick
    OnDrawItem = lbFriendsDrawItem
    OnMeasureItem = lbFriendsMeasureItem
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 506
    Width = 369
    Height = 54
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnOK: TButton
      Left = 164
      Top = 0
      Width = 80
      Height = 40
      Caption = #54869#51064
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 250
      Top = 0
      Width = 80
      Height = 40
      Caption = #52712#49548
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 280
    Top = 248
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Password=1234'
      'User_Name=root'
      'Database=chating app'
      'CharacterSet=utf8mb4'
      'DriverID=MySQL')
    LoginPrompt = False
    Left = 264
    Top = 296
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    VendorLib = 'C:\MYPROGRAM\delphi\mysql\mysql-5.7.33-win32\lib\libmysql.dll'
    Left = 208
    Top = 368
  end
end
