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
      Left = 600
      Top = 32
      Width = 201
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
    object BImportAll: TButton
      Left = 280
      Top = 32
      Width = 185
      Height = 25
      Caption = 'Import All'
      TabOrder = 3
      OnClick = BImportAllClick
    end
    object BExportAll: TButton
      Left = 472
      Top = 32
      Width = 105
      Height = 25
      Caption = 'Export All'
      TabOrder = 4
      OnClick = BExportAllClick
    end
  end
  object SBMain: TScrollBox
    Left = 0
    Top = 0
    Width = 920
    Height = 496
    VertScrollBar.Position = 86
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 1
    object PMain: TPanel
      Left = 0
      Top = -86
      Width = 833
      Height = 2500
      BevelOuter = bvNone
      TabOrder = 0
      object GBCreateDNA: TGroupBox
        Left = 8
        Top = 680
        Width = 777
        Height = 185
        Caption = ' 3 :: Create or import individual DNA string '
        TabOrder = 0
        object LCreateDNAStats: TLabel
          Left = 8
          Top = 164
          Width = 192
          Height = 13
          Caption = 'Status: No individual DNA string present'
        end
        object BCreateDNACreate: TButton
          Left = 8
          Top = 88
          Width = 193
          Height = 25
          Caption = 'Create'
          TabOrder = 0
          OnClick = BCreateDNACreateClick
        end
        object BCreateDNAExport: TButton
          Left = 408
          Top = 88
          Width = 273
          Height = 25
          Caption = 'Export'
          TabOrder = 1
          OnClick = BCreateDNAExportClick
        end
        object BCreateDNAImport: TButton
          Left = 208
          Top = 88
          Width = 193
          Height = 25
          Caption = 'Import'
          TabOrder = 2
          OnClick = BCreateDNAImportClick
        end
        object GBCreateDNAEdits: TGroupBox
          Left = 8
          Top = 16
          Width = 209
          Height = 65
          Caption = ' Edits '
          TabOrder = 3
          object LCreateDNAEditsLikeBefore: TLabel
            Left = 8
            Top = 16
            Width = 68
            Height = 13
            Caption = 'Edit likelihood:'
          end
          object LCreateDNAEditsContLikeBefore: TLabel
            Left = 8
            Top = 40
            Width = 130
            Height = 13
            Caption = 'Edit continuation likelihood:'
          end
          object LCreateDNAEditsLikeAfter: TLabel
            Left = 182
            Top = 16
            Width = 15
            Height = 13
            Caption = #8240
          end
          object LCreateDNAEditsContLikeAfter: TLabel
            Left = 182
            Top = 40
            Width = 15
            Height = 13
            Caption = #8240
          end
          object ECreateDNAEditsLike: TEdit
            Left = 80
            Top = 13
            Width = 97
            Height = 21
            TabOrder = 0
            Text = '50'
          end
          object ECreateDNAEditsContLike: TEdit
            Left = 142
            Top = 37
            Width = 35
            Height = 21
            TabOrder = 1
            Text = '200'
          end
        end
        object GBCreateDNAInserts: TGroupBox
          Left = 224
          Top = 16
          Width = 209
          Height = 65
          Caption = ' Inserts '
          TabOrder = 4
          object LCreateDNAInsertsLikeBefore: TLabel
            Left = 8
            Top = 16
            Width = 79
            Height = 13
            Caption = 'Insert likelihood:'
          end
          object LCreateDNAInsertsContLikeBefore: TLabel
            Left = 8
            Top = 40
            Width = 141
            Height = 13
            Caption = 'Insert continuation likelihood:'
          end
          object LCreateDNAInsertsLikeAfter: TLabel
            Left = 182
            Top = 16
            Width = 15
            Height = 13
            Caption = #8240
          end
          object LCreateDNAInsertsContLikeAfter: TLabel
            Left = 182
            Top = 40
            Width = 15
            Height = 13
            Caption = #8240
          end
          object ECreateDNAInsertsLike: TEdit
            Left = 91
            Top = 13
            Width = 89
            Height = 21
            TabOrder = 0
            Text = '10'
          end
          object ECreateDNAInsertsContLike: TEdit
            Left = 153
            Top = 37
            Width = 35
            Height = 21
            TabOrder = 1
            Text = '200'
          end
        end
        object GBCreateDNADeletions: TGroupBox
          Left = 440
          Top = 16
          Width = 209
          Height = 65
          Caption = ' Deletions '
          TabOrder = 5
          object LCreateDNADeletionsLikeBefore: TLabel
            Left = 8
            Top = 16
            Width = 81
            Height = 13
            Caption = 'Delete likelihood:'
          end
          object LCreateDNADeletionsContLikeBefore: TLabel
            Left = 8
            Top = 40
            Width = 143
            Height = 13
            Caption = 'Delete continuation likelihood:'
          end
          object LCreateDNADeletionsLikeAfter: TLabel
            Left = 182
            Top = 16
            Width = 15
            Height = 13
            Caption = #8240
          end
          object LCreateDNADeletionsContLikeAfter: TLabel
            Left = 182
            Top = 40
            Width = 15
            Height = 13
            Caption = #8240
          end
          object ECreateDNADeletionsLike: TEdit
            Left = 93
            Top = 13
            Width = 97
            Height = 21
            TabOrder = 0
            Text = '10'
          end
          object ECreateDNADeletionsContLike: TEdit
            Left = 155
            Top = 37
            Width = 35
            Height = 21
            TabOrder = 1
            Text = '200'
          end
        end
        object MCreateDNAres: TRichEdit
          Left = 8
          Top = 120
          Width = 673
          Height = 41
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 6
          WordWrap = False
          OnChange = MCreateDNAresChange
        end
      end
      object GBCreateReads: TGroupBox
        Left = 8
        Top = 872
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
          OnChange = ECreateReadsLengthChange
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
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
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
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
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
        Height = 193
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
        object LReferenceDNAStats: TLabel
          Left = 8
          Top = 172
          Width = 195
          Height = 13
          Caption = 'Status: No reference DNA string present'
        end
        object LReferenceDNAFormat: TLabel
          Left = 8
          Top = 103
          Width = 38
          Height = 13
          Caption = 'Format:'
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
          OnClick = CBReferenceDNAGraphClick
        end
        object MReferenceDNA: TMemo
          Left = 8
          Top = 128
          Width = 633
          Height = 41
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
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
        object CBReferenceDNAFormat: TComboBox
          Left = 56
          Top = 100
          Width = 697
          Height = 21
          ItemHeight = 13
          TabOrder = 7
          Text = 'STPU'
          OnChange = CBReferenceDNAFormatChange
          Items.Strings = (
            'STPU'
            'FASTA'
            'FASTG'
            'GFA')
        end
        object RBReferenceDNAGraphSnips: TRadioButton
          Left = 8
          Top = 78
          Width = 233
          Height = 17
          Caption = '1 :: non-nested single-char two-snips only'
          TabOrder = 8
          OnClick = RBReferenceDNAGraphSnipsClick
        end
        object RBReferenceDNAGraphFull: TRadioButton
          Left = 640
          Top = 78
          Width = 113
          Height = 17
          Caption = '4 :: full graph'
          TabOrder = 9
          OnClick = RBReferenceDNAGraphFullClick
        end
        object RBReferenceDNAGraphMultiSnips: TRadioButton
          Left = 248
          Top = 78
          Width = 225
          Height = 17
          Caption = '2 :: non-nested same-length multi-snips'
          TabOrder = 10
          OnClick = RBReferenceDNAGraphSnipsClick
        end
        object RBReferenceDNAGraphFullMultiSnips: TRadioButton
          Left = 472
          Top = 78
          Width = 161
          Height = 17
          Caption = '3 :: same-length multi-snips'
          TabOrder = 11
          OnClick = RBReferenceDNAGraphSnipsClick
        end
      end
      object GBPreProcessReference: TGroupBox
        Left = 8
        Top = 208
        Width = 777
        Height = 465
        Caption = ' 2 :: Pre-process reference DNA '
        TabOrder = 3
        object PPreProcessReferenceWOBWT: TPanel
          Left = 2
          Top = 240
          Width = 773
          Height = 223
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          object LPreProcessReferenceHorizonBefore: TLabel
            Left = 6
            Top = 8
            Width = 40
            Height = 13
            Caption = 'Horizon:'
          end
          object LPreProcessReferenceStats: TLabel
            Left = 6
            Top = 132
            Width = 190
            Height = 13
            Caption = 'Status: No pre-processing has occurred'
          end
          object LPreProcessReferenceOutRef: TLabel
            Left = 6
            Top = 160
            Width = 296
            Height = 13
            Caption = 'Adjusted reference for steps 5, 6 and 7 (but NOT for step 4):'
          end
          object LPreProcessReferenceHorizonAfter: TLabel
            Left = 108
            Top = 8
            Width = 46
            Height = 13
            Caption = 'basepairs'
          end
          object MPreProcessReference: TMemo
            Left = 6
            Top = 32
            Width = 633
            Height = 97
            Color = clWhite
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            PopupMenu = PMGeneral
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
            OnChange = MPreProcessReferenceChange
          end
          object MPreProcessReferenceOutRef: TMemo
            Left = 6
            Top = 176
            Width = 633
            Height = 41
            Color = clWhite
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            PopupMenu = PMGeneral
            ScrollBars = ssHorizontal
            TabOrder = 1
            WordWrap = False
          end
          object BPreProcessReference: TButton
            Left = 208
            Top = 2
            Width = 145
            Height = 25
            Caption = 'Pre-process'
            TabOrder = 2
            OnClick = BPreProcessReferenceClick
          end
          object BPreProcessReferenceImport: TButton
            Left = 360
            Top = 2
            Width = 161
            Height = 25
            Caption = 'Import'
            TabOrder = 3
            OnClick = BPreProcessReferenceImportClick
          end
          object BPreProcessReferenceExport: TButton
            Left = 528
            Top = 2
            Width = 153
            Height = 25
            Caption = 'Export'
            TabOrder = 4
            OnClick = BPreProcessReferenceExportClick
          end
          object EPreProcessReferenceHorizon: TEdit
            Left = 52
            Top = 5
            Width = 53
            Height = 21
            TabOrder = 5
            Text = '10'
            OnChange = EPreProcessReferenceHorizonChange
          end
        end
        object PPreProcessReferenceWBWT: TPanel
          Left = 2
          Top = 40
          Width = 773
          Height = 200
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object LPreProcessReferenceWBWTStats: TLabel
            Left = 6
            Top = 180
            Width = 190
            Height = 13
            Caption = 'Status: No pre-processing has occurred'
          end
          object MPreProcessReferenceSA: TMemo
            Left = 6
            Top = 80
            Width = 633
            Height = 97
            Color = clWhite
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            PopupMenu = PMGeneral
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
          object MPreProcessReferenceBWT: TMemo
            Left = 6
            Top = 32
            Width = 633
            Height = 41
            Color = clWhite
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
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
          object BPreProcessReferenceBWT: TButton
            Left = 8
            Top = 2
            Width = 361
            Height = 25
            Caption = 'Pre-process'
            TabOrder = 2
            OnClick = BPreProcessReferenceClick
          end
        end
        object CBuseBWT: TRadioButton
          Left = 504
          Top = 20
          Width = 241
          Height = 17
          Caption = 'Use Burrows-Wheeler Transform [Siren2014]'
          TabOrder = 2
          OnClick = CBuseBWTClick
        end
        object CBuseHashesNew: TRadioButton
          Left = 8
          Top = 20
          Width = 281
          Height = 17
          Caption = 'Use hashes [new]'
          Checked = True
          TabOrder = 3
          TabStop = True
          OnClick = CBuseBWTClick
        end
        object CBuseHashes: TRadioButton
          Left = 160
          Top = 20
          Width = 185
          Height = 17
          Caption = 'Use Hashes [Schneeberger2009]'
          TabOrder = 4
          OnClick = CBuseBWTClick
        end
        object CBuseBWTnew: TRadioButton
          Left = 360
          Top = 20
          Width = 241
          Height = 17
          Caption = 'Use Burrows-Wheeler Transform [new]'
          TabOrder = 5
          OnClick = CBuseBWTClick
        end
      end
      object GBAlignReads: TGroupBox
        Left = 8
        Top = 1096
        Width = 777
        Height = 225
        Caption = ' 5 :: Align reads to reference DNA '
        TabOrder = 4
        object LAlignReadsStatus: TLabel
          Left = 8
          Top = 204
          Width = 150
          Height = 13
          Caption = 'Status: Alignment not initialized'
        end
        object LAlignReadsLengthBefore: TLabel
          Left = 8
          Top = 48
          Width = 75
          Height = 13
          Caption = 'Interval length:'
        end
        object LAlignReadsLengthAfter: TLabel
          Left = 204
          Top = 48
          Width = 46
          Height = 13
          Caption = 'basepairs'
        end
        object LAlignReadsDBefore: TLabel
          Left = 272
          Top = 48
          Width = 158
          Height = 13
          Caption = 'Maximum amount of mismatches:'
          ParentShowHint = False
          ShowHint = True
        end
        object LAlignReadsDAfter: TLabel
          Left = 620
          Top = 48
          Width = 46
          Height = 13
          Caption = 'basepairs'
        end
        object LAlignReadsDMiddle: TLabel
          Left = 516
          Top = 48
          Width = 8
          Height = 13
          Caption = '+'
        end
        object MAlignReads: TMemo
          Left = 8
          Top = 104
          Width = 633
          Height = 97
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
          OnChange = MAlignReadsChange
        end
        object BAlignReads: TButton
          Left = 8
          Top = 74
          Width = 249
          Height = 25
          Caption = 'Align'
          TabOrder = 1
          OnClick = BAlignReadsClick
        end
        object BAlignReadsExport: TButton
          Left = 456
          Top = 74
          Width = 249
          Height = 25
          Caption = 'Export'
          TabOrder = 2
          OnClick = BAlignReadsExportClick
        end
        object EAlignReadsLength: TEdit
          Left = 84
          Top = 45
          Width = 117
          Height = 21
          TabOrder = 3
          Text = '10'
          OnChange = EAlignReadsLengthChange
        end
        object EAlignReadsD: TEdit
          Left = 436
          Top = 45
          Width = 69
          Height = 21
          TabOrder = 4
          Text = '1'
          OnChange = ECreateReadsLengthChange
        end
        object EAlignReadsK: TEdit
          Left = 532
          Top = 45
          Width = 77
          Height = 21
          TabOrder = 5
          Text = '3'
        end
        object CBusePigeonhole: TCheckBox
          Left = 8
          Top = 20
          Width = 289
          Height = 17
          Caption = 'Use Pigeonhole Algorithm to find imperfect matches'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
        object BAlignReadsImport: TButton
          Left = 224
          Top = 74
          Width = 273
          Height = 25
          Caption = 'Import'
          TabOrder = 7
          OnClick = BAlignReadsImportClick
        end
      end
      object GBAssembleDNA: TGroupBox
        Left = 8
        Top = 1328
        Width = 777
        Height = 249
        Caption = ' 6 :: Assemble full individual DNA string '
        TabOrder = 5
        object LAssembleDNAStatus: TLabel
          Left = 8
          Top = 92
          Width = 148
          Height = 13
          Caption = 'Status: Assembly not initialized'
        end
        object BAssembleDNA: TButton
          Left = 8
          Top = 18
          Width = 249
          Height = 25
          Caption = 'Assemble'
          TabOrder = 0
          OnClick = BAssembleDNAClick
        end
        object BAssembleDNAExport: TButton
          Left = 472
          Top = 18
          Width = 249
          Height = 25
          Caption = 'Export'
          TabOrder = 1
          OnClick = BAssembleDNAExportClick
        end
        object MAssembleDNA: TRichEdit
          Left = 8
          Top = 48
          Width = 673
          Height = 41
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 2
          WordWrap = False
          OnChange = MAssembleDNAChange
        end
        object BAssembleDNAShowAlign: TButton
          Left = 8
          Top = 112
          Width = 505
          Height = 25
          Caption = 'Directly Show Alignment'
          TabOrder = 3
          OnClick = BAssembleDNAShowAlignClick
        end
        object MAssembleDNAShowAlign: TMemo
          Left = 8
          Top = 144
          Width = 633
          Height = 97
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssBoth
          TabOrder = 4
          WordWrap = False
          OnChange = MAlignReadsChange
        end
        object BAssembleDNAImport: TButton
          Left = 224
          Top = 18
          Width = 273
          Height = 25
          Caption = 'Import'
          TabOrder = 5
          OnClick = BAssembleDNAImportClick
        end
      end
      object GBFillGaps: TGroupBox
        Left = 8
        Top = 1584
        Width = 777
        Height = 129
        Caption = 
          ' 7 :: Fill in the gaps using magic (that is, fill the gaps just ' +
          'plainly with the reference - only good for mismatches, baaad for' +
          ' indels!) '
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
          Width = 561
          Height = 13
          Caption = 
            'Actually, this step is a last-ditch attempt to somehow save the ' +
            'outcome. If at all possible, collect more reads instead.'
        end
        object BFillGaps: TButton
          Left = 8
          Top = 34
          Width = 249
          Height = 25
          Caption = 'Fill in the gaps'
          TabOrder = 0
          OnClick = BFillGapsClick
        end
        object BFillGapsExport: TButton
          Left = 464
          Top = 34
          Width = 249
          Height = 25
          Caption = 'Export'
          TabOrder = 1
          OnClick = BFillGapsExportClick
        end
        object MFillGaps: TRichEdit
          Left = 8
          Top = 64
          Width = 673
          Height = 41
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 2
          WordWrap = False
          OnChange = MFillGapsChange
        end
        object BFillGapsImport: TButton
          Left = 216
          Top = 34
          Width = 273
          Height = 25
          Caption = 'Import'
          TabOrder = 3
          OnClick = BFillGapsImportClick
        end
      end
      object GBAddNotes: TGroupBox
        Left = 8
        Top = 1968
        Width = 777
        Height = 193
        Caption = ' 9 :: Add some notes '
        TabOrder = 7
        object MAddNotes: TMemo
          Left = 8
          Top = 16
          Width = 633
          Height = 169
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
          OnChange = MAlignReadsChange
        end
      end
      object GBCallVariants: TGroupBox
        Left = 8
        Top = 1720
        Width = 777
        Height = 241
        Caption = ' 8 :: Call Variants '
        TabOrder = 8
        object LCallVariantsFRD: TLabel
          Left = 8
          Top = 48
          Width = 174
          Height = 13
          Caption = 'From aligned reads (step 5) directly:'
        end
        object LCallVariantsRAD: TLabel
          Left = 248
          Top = 48
          Width = 168
          Height = 13
          Caption = 'From assembled DNA (step 6 or 7):'
        end
        object LCallVariantsOV: TLabel
          Left = 496
          Top = 48
          Width = 229
          Height = 13
          Caption = 'Original variants (only known for artificial data):'
        end
        object MCallVariantsFRD: TMemo
          Left = 8
          Top = 64
          Width = 233
          Height = 169
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
        object BCallVariants: TButton
          Left = 8
          Top = 18
          Width = 249
          Height = 25
          Caption = 'Call the variants'
          TabOrder = 1
          OnClick = BCallVariantsClick
        end
        object BCallVariantsExport: TButton
          Left = 456
          Top = 18
          Width = 249
          Height = 25
          Caption = 'Export'
          TabOrder = 2
          OnClick = BCallVariantsExportClick
        end
        object BCallVariantsImport: TButton
          Left = 224
          Top = 18
          Width = 273
          Height = 25
          Caption = 'Import'
          TabOrder = 3
          OnClick = BCallVariantsImportClick
        end
        object MCallVariantsRAD: TMemo
          Left = 248
          Top = 64
          Width = 233
          Height = 169
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssBoth
          TabOrder = 4
          WordWrap = False
        end
        object MCallVariantsOV: TMemo
          Left = 496
          Top = 64
          Width = 233
          Height = 169
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = PMGeneral
          ScrollBars = ssBoth
          TabOrder = 5
          WordWrap = False
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
    Left = 40
    Top = 8
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
    object N3: TMenuItem
      Caption = '-'
    end
    object RefreshStatistics1: TMenuItem
      Caption = 'Refresh Statistics'
      OnClick = RefreshStatistics1Click
    end
  end
  object ODGeneral: TOpenDialog
    Filter = 'Main Central Control Files (*.mcc)|*.mcc|All Files (*.*)|*.*'
    Left = 72
    Top = 8
  end
  object SDGeneral: TSaveDialog
    Filter = 'All Files (*.*)|*.*'
    Left = 104
    Top = 8
  end
end
