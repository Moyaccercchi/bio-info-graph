object FOptions: TFOptions
  Left = 278
  Top = 137
  Width = 488
  Height = 147
  Caption = 'Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 313
    Height = 13
    Caption = 'Please specify the folder in which your python.exe can be found:'
  end
  object Button1: TButton
    Left = 224
    Top = 88
    Width = 123
    Height = 25
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 352
    Top = 88
    Width = 121
    Height = 25
    Caption = 'Save'
    TabOrder = 1
    OnClick = Button2Click
  end
  object EPythonPath: TEdit
    Left = 8
    Top = 32
    Width = 465
    Height = 21
    TabOrder = 2
  end
end
