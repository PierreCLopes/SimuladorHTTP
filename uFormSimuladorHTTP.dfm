object Form1: TForm1
  Left = 293
  Top = 110
  Caption = 'Form1'
  ClientHeight = 509
  ClientWidth = 684
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object LMetodo: TLabel
    Left = 8
    Top = 8
    Width = 36
    Height = 13
    Caption = 'M'#233'todo'
  end
  object LURL: TLabel
    Left = 63
    Top = 8
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object LBody: TLabel
    Left = 8
    Top = 126
    Width = 24
    Height = 13
    Caption = 'Body'
  end
  object LRequest: TLabel
    Left = 8
    Top = 256
    Width = 40
    Height = 13
    Caption = 'Request'
  end
  object LResponse: TLabel
    Left = 327
    Top = 256
    Width = 47
    Height = 13
    Caption = 'Response'
  end
  object LHeader: TLabel
    Left = 292
    Top = 54
    Width = 61
    Height = 23
    Caption = 'Header'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LKey: TLabel
    Left = 8
    Top = 80
    Width = 18
    Height = 13
    Caption = 'Key'
  end
  object LValue: TLabel
    Left = 327
    Top = 80
    Width = 26
    Height = 13
    Caption = 'Value'
  end
  object Metodo: TComboBox
    Left = 8
    Top = 27
    Width = 49
    Height = 21
    TabOrder = 0
  end
  object URL: TEdit
    Left = 63
    Top = 27
    Width = 610
    Height = 21
    TabOrder = 1
  end
  object Executar: TButton
    Left = 598
    Top = 479
    Width = 75
    Height = 25
    Caption = 'Executar'
    TabOrder = 2
    OnClick = ExecutarClick
  end
  object Body: TMemo
    Left = 8
    Top = 145
    Width = 665
    Height = 89
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Request: TMemo
    Left = 8
    Top = 275
    Width = 313
    Height = 198
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object Response: TMemo
    Left = 327
    Top = 275
    Width = 346
    Height = 198
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 5
    Touch.ParentTabletOptions = False
    Touch.TabletOptions = [toPressAndHold, toPenTapFeedback, toPenBarrelFeedback, toTouchUIForceOn, toTouchUIForceOff, toTouchSwitch, toFlicks, toSmoothScrolling, toFlickFallbackKeys]
  end
  object Key: TEdit
    Left = 8
    Top = 99
    Width = 313
    Height = 21
    TabOrder = 6
  end
  object Value: TEdit
    Left = 327
    Top = 99
    Width = 346
    Height = 21
    TabOrder = 7
  end
end
