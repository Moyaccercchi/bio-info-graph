object FMain: TFMain
  Left = 275
  Top = 137
  Width = 928
  Height = 533
  Caption = 'Main Central Control'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GBCreateDNA: TGroupBox
    Left = 8
    Top = 8
    Width = 777
    Height = 129
    Caption = ' Create random DNA string '
    TabOrder = 0
    object LCreateDNALength: TLabel
      Left = 8
      Top = 24
      Width = 37
      Height = 13
      Caption = 'Length:'
    end
    object LCreateDNALengthBasepairs: TLabel
      Left = 180
      Top = 24
      Width = 46
      Height = 13
      Caption = 'basepairs'
    end
    object LCreateDNAAlphabet: TLabel
      Left = 288
      Top = 24
      Width = 47
      Height = 13
      Caption = 'Alphabet:'
    end
    object ECreateDNALength: TEdit
      Left = 52
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '100'
    end
    object BCreateDNACreate: TButton
      Left = 8
      Top = 48
      Width = 273
      Height = 25
      Caption = 'Create'
      TabOrder = 1
      OnClick = BCreateDNACreateClick
    end
    object BCreateDNACopy: TButton
      Left = 288
      Top = 48
      Width = 273
      Height = 25
      Caption = 'Copy to Clipboard'
      TabOrder = 2
      OnClick = BCreateDNACopyClick
    end
    object MCreateDNAres: TMemo
      Left = 8
      Top = 80
      Width = 633
      Height = 41
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssHorizontal
      TabOrder = 3
      WordWrap = False
    end
    object ECreateDNAAlphabet: TEdit
      Left = 340
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 4
      Text = 'ACGT'
    end
  end
  object BClose: TButton
    Left = 512
    Top = 464
    Width = 265
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = BCloseClick
  end
  object GBCreateReads: TGroupBox
    Left = 8
    Top = 144
    Width = 777
    Height = 185
    Caption = ' Create random reads '
    TabOrder = 2
    object ECreateReadsLengthBefore: TLabel
      Left = 8
      Top = 24
      Width = 62
      Height = 13
      Caption = 'Read length:'
    end
    object ECreateReadsLengthAfter: TLabel
      Left = 204
      Top = 24
      Width = 46
      Height = 13
      Caption = 'basepairs'
    end
    object ECreateReadsAmountBefore: TLabel
      Left = 256
      Top = 24
      Width = 84
      Height = 13
      Caption = 'Amount of reads:'
    end
    object ECreateReadsMisProbBefore: TLabel
      Left = 404
      Top = 24
      Width = 101
      Height = 13
      Caption = 'Mismatch probability:'
    end
    object ECreateReadsMisProbAfter: TLabel
      Left = 640
      Top = 24
      Width = 11
      Height = 13
      Caption = '%'
    end
    object BGBCreateReadsCreate: TButton
      Left = 8
      Top = 48
      Width = 273
      Height = 25
      Caption = 'Create'
      TabOrder = 0
      OnClick = BGBCreateReadsCreateClick
    end
    object ECreateReadsAmount: TEdit
      Left = 348
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '20'
    end
    object ECreateReadsLength: TEdit
      Left = 76
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '20'
    end
    object ECreateReadsMisProb: TEdit
      Left = 516
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '0'
    end
    object MCreateReadsres: TMemo
      Left = 8
      Top = 80
      Width = 633
      Height = 97
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 4
      WordWrap = False
    end
  end
  object GBReconstructDNA: TGroupBox
    Left = 8
    Top = 336
    Width = 777
    Height = 105
    Caption = ' Reconstruct DNA '
    TabOrder = 3
  end
  object BOptions: TButton
    Left = 8
    Top = 464
    Width = 265
    Height = 25
    Caption = 'Options'
    TabOrder = 4
    OnClick = BOptionsClick
  end
  object TCheckExternals: TTimer
    Enabled = False
    OnTimer = TCheckExternalsTimer
    Left = 8
    Top = 8
  end
end
