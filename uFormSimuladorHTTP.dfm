object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 351
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
    Top = 206
    Width = 24
    Height = 13
    Caption = 'Body'
  end
  object LRequest: TLabel
    Left = 248
    Top = 206
    Width = 40
    Height = 13
    Caption = 'Request'
  end
  object LResponse: TLabel
    Left = 488
    Top = 206
    Width = 47
    Height = 13
    Caption = 'Response'
  end
  object LHeader: TLabel
    Left = 253
    Top = 54
    Width = 35
    Height = 13
    Caption = 'Header'
  end
  object LKey: TLabel
    Left = 8
    Top = 72
    Width = 18
    Height = 13
    Caption = 'Key'
  end
  object LValue: TLabel
    Left = 278
    Top = 72
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
    Width = 376
    Height = 21
    TabOrder = 1
  end
  object Executar: TButton
    Left = 638
    Top = 318
    Width = 75
    Height = 25
    Caption = 'Executar'
    TabOrder = 2
    OnClick = ExecutarClick
  end
  object Body: TMemo
    Left = 8
    Top = 225
    Width = 185
    Height = 89
    TabOrder = 3
  end
  object Request: TMemo
    Left = 248
    Top = 224
    Width = 185
    Height = 89
    TabOrder = 4
  end
  object Response: TMemo
    Left = 488
    Top = 224
    Width = 185
    Height = 89
    TabOrder = 5
  end
  object Key: TEdit
    Left = 8
    Top = 91
    Width = 257
    Height = 21
    TabOrder = 6
  end
  object Edit1: TEdit
    Left = 278
    Top = 91
    Width = 257
    Height = 21
    TabOrder = 7
  end
end
