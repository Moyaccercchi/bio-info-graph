object FMain: TFMain
  Left = 275
  Top = 69
  Width = 928
  Height = 601
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
  object PHUDBtm: TPanel
    Left = 0
    Top = 496
    Width = 920
    Height = 78
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object LVersion: TLabel
      Left = 8
      Top = 60
      Width = 3
      Height = 13
    end
    object BOptions: TButton
      Left = 8
      Top = 32
      Width = 265
      Height = 25
      Caption = 'Options'
      TabOrder = 0
      OnClick = BOptionsClick
    end
    object BClose: TButton
      Left = 480
      Top = 32
      Width = 265
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = BCloseClick
    end
    object SBAllMemos: TScrollBar
      Left = 16
      Top = 8
      Width = 633
      Height = 16
      PageSize = 0
      TabOrder = 2
      OnScroll = SBAllMemosScroll
    end
  end
  object SBMain: TScrollBox
    Left = 0
    Top = 0
    Width = 920
    Height = 496
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 1
    object PMain: TPanel
      Left = 0
      Top = 0
      Width = 833
      Height = 1121
      BevelOuter = bvNone
      Caption = 'a'
      TabOrder = 0
      object GBCreateDNA: TGroupBox
        Left = 8
        Top = 304
        Width = 777
        Height = 121
        Caption = ' 3 :: Create or import individual DNA string '
        TabOrder = 0
        object LCreateDNAStats: TLabel
          Left = 8
          Top = 100
          Width = 192
          Height = 13
          Caption = 'Status: No individual DNA string present'
        end
        object BCreateDNACreate: TButton
          Left = 8
          Top = 24
          Width = 193
          Height = 25
          Caption = 'Create'
          TabOrder = 0
          OnClick = BCreateDNACreateClick
        end
        object BCreateDNAExport: TButton
          Left = 408
          Top = 24
          Width = 273
          Height = 25
          Caption = 'Export'
          TabOrder = 1
          OnClick = BCreateDNAExportClick
        end
        object MCreateDNAres: TMemo
          Left = 8
          Top = 56
          Width = 633
          Height = 41
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssHorizontal
          TabOrder = 2
          WordWrap = False
          OnChange = MCreateDNAresChange
        end
        object BCreateDNAImport: TButton
          Left = 208
          Top = 24
          Width = 193
          Height = 25
          Caption = 'Import'
          TabOrder = 3
          OnClick = BCreateDNAImportClick
        end
      end
      object GBCreateReads: TGroupBox
        Left = 8
        Top = 432
        Width = 777
        Height = 217
        Caption = ' 4 :: Create or import reads '
        TabOrder = 1
        object LCreateReadsLengthBefore: TLabel
          Left = 8
          Top = 24
          Width = 62
          Height = 13
          Caption = 'Read length:'
        end
        object LCreateReadsLengthAfter: TLabel
          Left = 204
          Top = 24
          Width = 46
          Height = 13
          Caption = 'basepairs'
        end
        object LCreateReadsAmountBefore: TLabel
          Left = 256
          Top = 24
          Width = 84
          Height = 13
          Caption = 'Amount of reads:'
        end
        object LCreateReadsMisProbBefore: TLabel
          Left = 404
          Top = 24
          Width = 101
          Height = 13
          Caption = 'Mismatch probability:'
        end
        object LCreateReadsMisProbAfter: TLabel
          Left = 640
          Top = 24
          Width = 11
          Height = 13
          Caption = '%'
        end
        object LCreateReadsStatus: TLabel
          Left = 8
          Top = 196
          Width = 121
          Height = 13
          Caption = 'Status: No reads created'
        end
        object LCreateReadsMres: TLabel
          Left = 8
          Top = 76
          Width = 34
          Height = 13
          Caption = 'Reads:'
        end
        object LCreateReadsMpos: TLabel
          Left = 336
          Top = 76
          Width = 275
          Height = 13
          Caption = 'Original locations of reads (only known for artificial data):'
        end
        object BCreateReadsCreate: TButton
          Left = 8
          Top = 48
          Width = 273
          Height = 25
          Caption = 'Create'
          TabOrder = 0
          OnClick = BCreateReadsCreateClick
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
          Top = 94
          Width = 321
          Height = 97
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 4
          WordWrap = False
          OnChange = MCreateReadsresChange
        end
        object BCreateReadsImport: TButton
          Left = 200
          Top = 48
          Width = 273
          Height = 25
          Caption = 'Import'
          TabOrder = 5
          OnClick = BCreateReadsImportClick
        end
        object BCreateReadsExport: TButton
          Left = 408
          Top = 48
          Width = 273
          Height = 25
          Caption = 'Export'
          TabOrder = 6
          OnClick = BCreateReadsExportClick
        end
        object MCreateReadspos: TMemo
          Left = 336
          Top = 94
          Width = 321
          Height = 97
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssBoth
          TabOrder = 7
          WordWrap = False
          OnChange = MCreateReadsposChange
        end
      end
      object GBReferenceDNA: TGroupBox
        Left = 8
        Top = 8
        Width = 777
        Height = 129
        Caption = ' 1 :: Create or import reference DNA string '
        TabOrder = 2
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
        object CBReferenceDNAGraph: TCheckBox
          Left = 8
          Top = 52
          Width = 153
          Height = 17
          Caption = 'Use graph, not string'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object MReferenceDNA: TMemo
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
          PopupMenu = PMGeneral
          ScrollBars = ssHorizontal
          TabOrder = 1
          WordWrap = False
          OnChange = MReferenceDNAChange
        end
        object BReferenceDNACreate: TButton
          Left = 184
          Top = 48
          Width = 185
          Height = 25
          Caption = 'Create'
          TabOrder = 2
          OnClick = BReferenceDNACreateClick
        end
        object BReferenceDNAImport: TButton
          Left = 376
          Top = 48
          Width = 185
          Height = 25
          Caption = 'Import'
          TabOrder = 3
          OnClick = BReferenceDNAImportClick
        end
        object BReferenceDNAExport: TButton
          Left = 568
          Top = 48
          Width = 185
          Height = 25
          Caption = 'Export'
          TabOrder = 4
          OnClick = BReferenceDNAExportClick
        end
        object ECreateDNALength: TEdit
          Left = 52
          Top = 21
          Width = 121
          Height = 21
          TabOrder = 5
          Text = '100'
        end
        object ECreateDNAAlphabet: TEdit
          Left = 340
          Top = 21
          Width = 121
          Height = 21
          TabOrder = 6
          Text = 'ACGT'
        end
      end
      object GBPreProcessReference: TGroupBox
        Left = 8
        Top = 144
        Width = 777
        Height = 153
        Caption = ' 2 :: Pre-process reference DNA '
        TabOrder = 3
        object LPreProcessReferenceHorizonBefore: TLabel
          Left = 8
          Top = 24
          Width = 40
          Height = 13
          Caption = 'Horizon:'
        end
        object LPreProcessReferenceHorizonAfter: TLabel
          Left = 108
          Top = 24
          Width = 46
          Height = 13
          Caption = 'basepairs'
        end
        object BPreProcessReference: TButton
          Left = 208
          Top = 18
          Width = 145
          Height = 25
          Caption = 'Pre-process'
          TabOrder = 0
          OnClick = BPreProcessReferenceClick
        end
        object MPreProcessReference: TMemo
          Left = 8
          Top = 48
          Width = 633
          Height = 97
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
        end
        object BPreProcessReferenceImport: TButton
          Left = 360
          Top = 18
          Width = 161
          Height = 25
          Caption = 'Import'
          TabOrder = 2
          OnClick = BPreProcessReferenceImportClick
        end
        object BPreProcessReferenceExport: TButton
          Left = 528
          Top = 18
          Width = 153
          Height = 25
          Caption = 'Export'
          TabOrder = 3
          OnClick = BPreProcessReferenceExportClick
        end
        object EPreProcessReferenceHorizon: TEdit
          Left = 52
          Top = 21
          Width = 53
          Height = 21
          TabOrder = 4
          Text = '10'
          OnChange = EPreProcessReferenceHorizonChange
        end
      end
      object GBAlignReads: TGroupBox
        Left = 8
        Top = 656
        Width = 777
        Height = 201
        Caption = ' 5 :: Align reads to reference DNA '
        TabOrder = 4
        object LAlignReadsStatus: TLabel
          Left = 8
          Top = 180
          Width = 150
          Height = 13
          Caption = 'Status: Alignment not initialized'
        end
        object LAlignReadsLengthBefore: TLabel
          Left = 8
          Top = 24
          Width = 75
          Height = 13
          Caption = 'Interval length:'
        end
        object LAlignReadsLengthAfter: TLabel
          Left = 204
          Top = 24
          Width = 46
          Height = 13
          Caption = 'basepairs'
        end
        object LAlignReadsDBefore: TLabel
          Left = 272
          Top = 24
          Width = 158
          Height = 13
          Caption = 'Maximum amount of mismatches:'
        end
        object LAlignReadsDAfter: TLabel
          Left = 564
          Top = 24
          Width = 46
          Height = 13
          Caption = 'basepairs'
        end
        object MAlignReads: TMemo
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
          PopupMenu = PMGeneral
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
          OnChange = MAlignReadsChange
        end
        object BAlignReads: TButton
          Left = 8
          Top = 50
          Width = 249
          Height = 25
          Caption = 'Align'
          TabOrder = 1
          OnClick = BAlignReadsClick
        end
        object BAlignReadsExport: TButton
          Left = 264
          Top = 50
          Width = 249
          Height = 25
          Caption = 'Export'
          TabOrder = 2
          OnClick = BAlignReadsExportClick
        end
        object EAlignReadsLength: TEdit
          Left = 84
          Top = 21
          Width = 117
          Height = 21
          TabOrder = 3
          Text = '10'
          OnChange = EAlignReadsLengthChange
        end
        object EAlignReadsD: TEdit
          Left = 436
          Top = 21
          Width = 121
          Height = 21
          TabOrder = 4
          Text = '1'
        end
      end
      object GBAssembleDNA: TGroupBox
        Left = 8
        Top = 864
        Width = 777
        Height = 113
        Caption = ' 6 :: Assemble full individual DNA string '
        TabOrder = 5
        object LAssembleDNAStatus: TLabel
          Left = 8
          Top = 92
          Width = 148
          Height = 13
          Caption = 'Status: Assembly not initialized'
        end
        object MAssembleDNA: TMemo
          Left = 8
          Top = 48
          Width = 633
          Height = 41
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssHorizontal
          TabOrder = 0
          WordWrap = False
          OnChange = MAssembleDNAChange
        end
        object BAssembleDNA: TButton
          Left = 8
          Top = 18
          Width = 249
          Height = 25
          Caption = 'Assemble'
          TabOrder = 1
          OnClick = BAssembleDNAClick
        end
        object BAssembleDNAExport: TButton
          Left = 264
          Top = 18
          Width = 249
          Height = 25
          Caption = 'Export'
          TabOrder = 2
          OnClick = BAssembleDNAExportClick
        end
      end
      object GBFillGaps: TGroupBox
        Left = 8
        Top = 984
        Width = 777
        Height = 129
        Caption = ' 7 :: Fill in the gaps using magic '
        TabOrder = 6
        object LFillGapsStatus: TLabel
          Left = 8
          Top = 108
          Width = 148
          Height = 13
          Caption = 'Status: Assembly not initialized'
        end
        object LFillGapsTop: TLabel
          Left = 8
          Top = 16
          Width = 527
          Height = 13
          Caption = 
            'Actually, this step will DRASTICALLY DETERIORATE the outcome. If' +
            ' at all possible, collect more reads instead.'
        end
        object MFillGaps: TMemo
          Left = 8
          Top = 64
          Width = 633
          Height = 41
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssHorizontal
          TabOrder = 0
          WordWrap = False
          OnChange = MFillGapsChange
        end
        object BFillGaps: TButton
          Left = 8
          Top = 34
          Width = 249
          Height = 25
          Caption = 'Fill in the gaps'
          TabOrder = 1
          OnClick = BFillGapsClick
        end
        object BFillGapsExport: TButton
          Left = 264
          Top = 34
          Width = 249
          Height = 25
          Caption = 'Export'
          TabOrder = 2
          OnClick = BFillGapsExportClick
        end
      end
    end
  end
  object TCheckExternals: TTimer
    Enabled = False
    OnTimer = TCheckExternalsTimer
    Left = 8
    Top = 8
  end
  object PMGeneral: TPopupMenu
    Left = 8
    Top = 40
    object CopyAll1: TMenuItem
      Caption = 'Copy All'
      ShortCut = 16451
      OnClick = CopyAll1Click
    end
    object CutAll1: TMenuItem
      Caption = 'Cut All'
      ShortCut = 16472
      OnClick = CutAll1Click
    end
    object PasteAll1: TMenuItem
      Caption = 'Paste All'
      ShortCut = 16470
      OnClick = PasteAll1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object CopySelection1: TMenuItem
      Caption = 'Copy Selection'
      OnClick = CopySelection1Click
    end
    object CutSelection1: TMenuItem
      Caption = 'Cut Selection'
      OnClick = CutSelection1Click
    end
    object PasteSelection1: TMenuItem
      Caption = 'Paste Selection'
      OnClick = PasteSelection1Click
    end
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = SelectAll1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Undo1: TMenuItem
      Caption = 'Undo'
      ShortCut = 16474
      OnClick = Undo1Click
    end
  end
  object ODGeneral: TOpenDialog
    Filter = 'All Files (*.*)|*.*'
    Left = 8
    Top = 72
  end
  object SDGeneral: TSaveDialog
    Filter = 'All Files (*.*)|*.*'
    Left = 8
    Top = 104
  end
end
