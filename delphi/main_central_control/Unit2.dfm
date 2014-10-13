object FOptions: TFOptions
  Left = 278
  Top = 137
  Width = 488
  Height = 250
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
  object PHUDBtm: TPanel
    Left = 0
    Top = 182
    Width = 480
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 224
      Top = 8
      Width = 123
      Height = 25
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 352
      Top = 8
      Width = 121
      Height = 25
      Caption = 'Save'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object GBEnvironment: TGroupBox
    Left = 8
    Top = 8
    Width = 465
    Height = 73
    Caption = ' Environment '
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 313
      Height = 13
      Caption = 'Please specify the folder in which your python.exe can be found:'
    end
    object EPythonPath: TEdit
      Left = 8
      Top = 40
      Width = 449
      Height = 21
      TabOrder = 0
    end
  end
  object GBReset: TGroupBox
    Left = 8
    Top = 88
    Width = 465
    Height = 89
    Caption = ' Reset Settings '
    TabOrder = 2
    object Button3: TButton
      Left = 8
      Top = 24
      Width = 449
      Height = 25
      Caption = 'Reset to manually solvable problem'
      TabOrder = 0
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 8
      Top = 56
      Width = 449
      Height = 25
      Caption = 'Reset to highly simplified problem'
      TabOrder = 1
      OnClick = Button4Click
    end
  end
end
