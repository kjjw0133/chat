object Form12: TForm12
  Left = 0
  Top = 0
  Caption = 'Form12'
  ClientHeight = 492
  ClientWidth = 754
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Button2: TButton
    Left = 121
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Start/Stop'
    TabOrder = 0
    OnClick = Button2Click
  end
  object RichEdit1: TRichEdit
    Left = 40
    Top = 127
    Width = 338
    Height = 241
    Font.Charset = HANGEUL_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Lines.Strings = (
      'RichEdit1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ServerSocket1: TServerSocket
    Active = False
    Port = 8080
    ServerType = stNonBlocking
    OnClientRead = ServerSocket1ClientRead
    Left = 576
    Top = 80
  end
end
