unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellApi, ExtCtrls, registry, Menus, MoyaUtils, Math,
  ComCtrls;

type
  TCallbackMethod = procedure of object;

  TFMain = class(TForm)
    TCheckExternals: TTimer;
    PHUDBtm: TPanel;
    BOptions: TButton;
    BClose: TButton;
    SBMain: TScrollBox;
    PMain: TPanel;
    GBCreateDNA: TGroupBox;
    BCreateDNACreate: TButton;
    BCreateDNAExport: TButton;
    BCreateDNAImport: TButton;
    GBCreateReads: TGroupBox;
    LCreateReadsLengthBefore: TLabel;
    LCreateReadsLengthAfter: TLabel;
    LCreateReadsAmountBefore: TLabel;
    LCreateReadsMisProbBefore: TLabel;
    LCreateReadsMisProbAfter: TLabel;
    BCreateReadsCreate: TButton;
    ECreateReadsAmount: TEdit;
    ECreateReadsLength: TEdit;
    ECreateReadsMisProb: TEdit;
    MCreateReadsres: TMemo;
    GBReferenceDNA: TGroupBox;
    CBReferenceDNAGraph: TCheckBox;
    MReferenceDNA: TMemo;
    BReferenceDNACreate: TButton;
    PMGeneral: TPopupMenu;
    CopyAll1: TMenuItem;
    PasteAll1: TMenuItem;
    N1: TMenuItem;
    CopySelection1: TMenuItem;
    PasteSelection1: TMenuItem;
    N2: TMenuItem;
    Undo1: TMenuItem;
    LVersion: TLabel;
    BCreateReadsImport: TButton;
    BCreateReadsExport: TButton;
    BReferenceDNAImport: TButton;
    BReferenceDNAExport: TButton;
    ODGeneral: TOpenDialog;
    SDGeneral: TSaveDialog;
    LCreateDNAStats: TLabel;
    GBPreProcessReference: TGroupBox;
    GBAlignReads: TGroupBox;
    GBAssembleDNA: TGroupBox;
    MAlignReads: TMemo;
    BAlignReads: TButton;
    BAssembleDNA: TButton;
    LAssembleDNAStatus: TLabel;
    SelectAll1: TMenuItem;
    CutAll1: TMenuItem;
    CutSelection1: TMenuItem;
    BAlignReadsExport: TButton;
    BAssembleDNAExport: TButton;
    LCreateDNALength: TLabel;
    ECreateDNALength: TEdit;
    LCreateDNALengthBasepairs: TLabel;
    LCreateDNAAlphabet: TLabel;
    ECreateDNAAlphabet: TEdit;
    MCreateReadspos: TMemo;
    LCreateReadsStatus: TLabel;
    LCreateReadsMres: TLabel;
    LCreateReadsMpos: TLabel;
    LAlignReadsStatus: TLabel;
    LAlignReadsLengthBefore: TLabel;
    EAlignReadsLength: TEdit;
    LAlignReadsLengthAfter: TLabel;
    LAlignReadsDBefore: TLabel;
    EAlignReadsD: TEdit;
    LAlignReadsDAfter: TLabel;
    SBAllMemos: TScrollBar;
    GBFillGaps: TGroupBox;
    LFillGapsStatus: TLabel;
    BFillGaps: TButton;
    BFillGapsExport: TButton;
    LFillGapsTop: TLabel;
    GBCreateDNAEdits: TGroupBox;
    LCreateDNAEditsLikeBefore: TLabel;
    LCreateDNAEditsContLikeBefore: TLabel;
    ECreateDNAEditsLike: TEdit;
    LCreateDNAEditsLikeAfter: TLabel;
    ECreateDNAEditsContLike: TEdit;
    LCreateDNAEditsContLikeAfter: TLabel;
    LReferenceDNAStats: TLabel;
    GBCreateDNAInserts: TGroupBox;
    LCreateDNAInsertsLikeBefore: TLabel;
    LCreateDNAInsertsContLikeBefore: TLabel;
    LCreateDNAInsertsLikeAfter: TLabel;
    LCreateDNAInsertsContLikeAfter: TLabel;
    ECreateDNAInsertsLike: TEdit;
    ECreateDNAInsertsContLike: TEdit;
    GBCreateDNADeletions: TGroupBox;
    LCreateDNADeletionsLikeBefore: TLabel;
    LCreateDNADeletionsContLikeBefore: TLabel;
    LCreateDNADeletionsLikeAfter: TLabel;
    LCreateDNADeletionsContLikeAfter: TLabel;
    ECreateDNADeletionsLike: TEdit;
    ECreateDNADeletionsContLike: TEdit;
    N3: TMenuItem;
    RefreshStatistics1: TMenuItem;
    LAlignReadsDMiddle: TLabel;
    EAlignReadsK: TEdit;
    LReferenceDNAFormat: TLabel;
    CBReferenceDNAFormat: TComboBox;
    MCreateDNAres: TRichEdit;
    MFillGaps: TRichEdit;
    MAssembleDNA: TRichEdit;
    RBReferenceDNAGraphSnips: TRadioButton;
    RBReferenceDNAGraphFull: TRadioButton;
    RBReferenceDNAGraphMultiSnips: TRadioButton;
    RBReferenceDNAGraphFullMultiSnips: TRadioButton;
    PPreProcessReferenceWOBWT: TPanel;
    LPreProcessReferenceHorizonBefore: TLabel;
    MPreProcessReference: TMemo;
    LPreProcessReferenceStats: TLabel;
    LPreProcessReferenceOutRef: TLabel;
    MPreProcessReferenceOutRef: TMemo;
    BPreProcessReference: TButton;
    BPreProcessReferenceImport: TButton;
    BPreProcessReferenceExport: TButton;
    LPreProcessReferenceHorizonAfter: TLabel;
    EPreProcessReferenceHorizon: TEdit;
    PPreProcessReferenceWBWT: TPanel;
    LPreProcessReferenceWBWTStats: TLabel;
    MPreProcessReferenceSA: TMemo;
    MPreProcessReferenceBWT: TMemo;
    BPreProcessReferenceBWT: TButton;
    CBusePigeonhole: TCheckBox;
    CBuseBWT: TRadioButton;
    CBuseHashesNew: TRadioButton;
    CBuseHashes: TRadioButton;
    BImportAll: TButton;
    BExportAll: TButton;
    BAssembleDNAShowAlign: TButton;
    MAssembleDNAShowAlign: TMemo;
    GBAddNotes: TGroupBox;
    MAddNotes: TMemo;
    GBCallVariants: TGroupBox;
    MCallVariantsFRD: TMemo;
    BCallVariants: TButton;
    BCallVariantsExport: TButton;
    BAlignReadsImport: TButton;
    BAssembleDNAImport: TButton;
    BFillGapsImport: TButton;
    BCallVariantsImport: TButton;
    LCallVariantsFRD: TLabel;
    LCallVariantsRAD: TLabel;
    LCallVariantsOV: TLabel;
    MCallVariantsRAD: TMemo;
    MCallVariantsOV: TMemo;
    CBuseBWTnew: TRadioButton;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure BCreateDNACreateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BCreateReadsCreateClick(Sender: TObject);
    procedure TCheckExternalsTimer(Sender: TObject);
    procedure BOptionsClick(Sender: TObject);
    procedure BReferenceDNACreateClick(Sender: TObject);
    procedure CopyAll1Click(Sender: TObject);
    procedure PasteAll1Click(Sender: TObject);
    procedure CopySelection1Click(Sender: TObject);
    procedure PasteSelection1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure BReferenceDNAImportClick(Sender: TObject);
    procedure BCreateDNAImportClick(Sender: TObject);
    procedure BCreateReadsImportClick(Sender: TObject);
    procedure BReferenceDNAExportClick(Sender: TObject);
    procedure BCreateDNAExportClick(Sender: TObject);
    procedure BCreateReadsExportClick(Sender: TObject);
    procedure MCreateDNAresChange(Sender: TObject);
    procedure MReferenceDNAChange(Sender: TObject);
    procedure BPreProcessReferenceClick(Sender: TObject);
    procedure BAlignReadsClick(Sender: TObject);
    procedure BAssembleDNAClick(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure CutAll1Click(Sender: TObject);
    procedure CutSelection1Click(Sender: TObject);
    procedure BAlignReadsExportClick(Sender: TObject);
    procedure BAssembleDNAExportClick(Sender: TObject);
    procedure BPreProcessReferenceImportClick(Sender: TObject);
    procedure BPreProcessReferenceExportClick(Sender: TObject);
    procedure MAssembleDNAChange(Sender: TObject);
    procedure EPreProcessReferenceHorizonChange(Sender: TObject);
    procedure MCreateReadsresChange(Sender: TObject);
    procedure MCreateReadsposChange(Sender: TObject);
    procedure MAlignReadsChange(Sender: TObject);
    procedure EAlignReadsLengthChange(Sender: TObject);
    procedure SBAllMemosScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure BFillGapsExportClick(Sender: TObject);
    procedure MFillGapsChange(Sender: TObject);
    procedure BFillGapsClick(Sender: TObject);
    procedure MPreProcessReferenceChange(Sender: TObject);
    procedure RefreshStatistics1Click(Sender: TObject);
    procedure CBReferenceDNAFormatChange(Sender: TObject);
    procedure CBReferenceDNAGraphClick(Sender: TObject);
    procedure ECreateReadsLengthChange(Sender: TObject);
    procedure RBReferenceDNAGraphSnipsClick(Sender: TObject);
    procedure RBReferenceDNAGraphFullClick(Sender: TObject);
    procedure CBuseBWTClick(Sender: TObject);
    procedure BPreProcessReferenceBWTClick(Sender: TObject);
    procedure BExportAllClick(Sender: TObject);
    procedure BImportAllClick(Sender: TObject);
    procedure BAssembleDNAShowAlignClick(Sender: TObject);
    procedure BCallVariantsExportClick(Sender: TObject);
    procedure BAlignReadsImportClick(Sender: TObject);
    procedure BAssembleDNAImportClick(Sender: TObject);
    procedure BFillGapsImportClick(Sender: TObject);
    procedure BCallVariantsImportClick(Sender: TObject);
    procedure BCallVariantsClick(Sender: TObject);
  private
    function copyend(str, substr: string; depth: Integer): string;
    function createRandomDNAString: string;
    function python_path_set: Boolean;                                                 
    function generateSTPUstringReference(len, alen: Integer; alphabet: string): string;
    function generateSTPUgraphReference(len, alen: Integer; alphabet: string): string;
    function generateSTPUgraphReference_S(len, alen: Integer; alphabet: string): string;
    function generateSTPUgraphReference_M(len, alen: Integer; alphabet: string): string;
    function generateSTPUgraphReference_FM(len, alen: Integer; alphabet: string; P_graphless: Integer): string;
    function generateSTPUgraphReference_F(len, alen: Integer; alphabet: string): string;
    function generateFASTAstringReference(len, alen: Integer; alphabet: string): string;
    function generateFASTGstringReference(len, alen: Integer; alphabet: string): string;
    function generateFASTGgraphReference(len, alen: Integer; alphabet: string): string;
    function generateFASTGgraphReference_S(len, alen: Integer; alphabet: string): string;
    function generateFASTGgraphReference_M(len, alen: Integer; alphabet: string): string;
    function generateFASTGgraphReference_F(len, alen: Integer; alphabet: string): string;
    function generateGFAstringReference(len, alen: Integer; alphabet: string): string;
    function generateGFAgraphReference(len, alen: Integer; alphabet: string): string;
    function generateGFAgraphReference_S(len, alen: Integer; alphabet: string): string;
    function generateGFAgraphReference_M(len, alen: Integer; alphabet: string): string;
    function generateGFAgraphReference_F(len, alen: Integer; alphabet: string): string;
    function countSTPUgraphAverageLength(graph: string): Extended;
    function countAlternatives(graph: string): Extended;
    procedure CallbackCreateReadsCreated;
    procedure CallbackPreProcessReference;
    procedure CallbackAlignReads;
    procedure CallbackAssembleDNA;
    procedure RefreshReferenceDNAStats;
    procedure RefreshAssembleDNAStats;
    procedure RefreshCreateDNAStats;
    procedure RefreshCreateReadsStats;
    procedure RefreshAlignReadsStats;
    procedure RefreshFillGapsStats;
    procedure RefreshPreProcessReferenceStats;
    procedure SaveDAB;
    procedure LoadDAB;
    procedure resetMemoHeights;
    procedure resetInterface;
    procedure runInPython(folder, input: string; callOnFinish: TCallbackMethod);
    procedure RefreshAssemblyStats(resLabel: TLabel; assemblyMemo: TRichEdit);
    procedure clearEverything;  
    procedure SaveFromMemo(varm: TCustomMemo; filename: string);
    procedure LoadIntoMemo(varm: TCustomMemo; filename: string);
  public
    procedure SaveOptions;
    procedure ResetTo(difficulty: Integer);
  end;

const
  version = '0020';
  verdate = '26. 6. 2015';

var
  FMain: TFMain;       
  Path, MainPath, PythonPath, TCheckExternal_FilePath, lastMAssembleDNAtext,
  lastMFillGapstext, AssembleDNA_quality: string;
  TCheckExternal_Call: TCallbackMethod;
  MCreateReadsres_reacttoonchange, MAssembleDNA_reacttoonchange,
  MAlignReads_reacttoonchange, MPreProcessReference_reacttoonchange,
  pauseBatchfiles, showPreProcessReferenceOutRef: Boolean;

implementation

uses Unit2;

{$R *.dfm}

procedure TFMain.LoadDAB;
var regist: TRegistry;
begin
  regist := TRegistry.Create;
  try
    regist.RootKey := HKEY_CURRENT_USER;

    regist.OpenKey('Software\moyaccercchi\BioInfoGraph\MainCentralControl', true);

    if regist.ValueExists('fmain.left') then
      FMain.Left := StrtoInt(regist.ReadString('fmain.left'))
    else
      FMain.Left := (screen.Width - FMain.Width) div 2;

    if regist.ValueExists('fmain.top') then
      FMain.Top := StrtoInt(regist.ReadString('fmain.top'))
    else
      FMain.Top := (screen.Height - FMain.Height) div 2;

    if regist.ValueExists('fmain.width') then
      FMain.Width := StrtoInt(regist.ReadString('fmain.width'));

    if regist.ValueExists('fmain.height') then
      FMain.Height := StrtoInt(regist.ReadString('fmain.height'));

    if regist.ValueExists('paths.python') then
      PythonPath := regist.ReadString('paths.python')
    else
      PythonPath := '';

    if regist.ValueExists('CBReferenceDNAGraph.checked') then
      CBReferenceDNAGraph.Checked := Boolean(StrToInt(regist.ReadString('CBReferenceDNAGraph.checked')));

    if regist.ValueExists('ECreateDNALength.Text') then
      ECreateDNALength.Text := regist.ReadString('ECreateDNALength.Text');

    if regist.ValueExists('ECreateDNAAlphabet.Text') then
      ECreateDNAAlphabet.Text := regist.ReadString('ECreateDNAAlphabet.Text');

    if regist.ValueExists('EPreProcessReferenceHorizon.Text') then
      EPreProcessReferenceHorizon.Text := regist.ReadString('EPreProcessReferenceHorizon.Text');

    if regist.ValueExists('ECreateReadsLength.Text') then
      ECreateReadsLength.Text := regist.ReadString('ECreateReadsLength.Text');

    if regist.ValueExists('ECreateReadsAmount.Text') then
      ECreateReadsAmount.Text := regist.ReadString('ECreateReadsAmount.Text');

    if regist.ValueExists('ECreateReadsMisProb.Text') then
      ECreateReadsMisProb.Text := regist.ReadString('ECreateReadsMisProb.Text');

    if regist.ValueExists('EAlignReadsLength.Text') then
      EAlignReadsLength.Text := regist.ReadString('EAlignReadsLength.Text');

    if regist.ValueExists('EAlignReadsD.Text') then
      EAlignReadsD.Text := regist.ReadString('EAlignReadsD.Text');

    if regist.ValueExists('EAlignReadsK.Text') then
      EAlignReadsK.Text := regist.ReadString('EAlignReadsK.Text');

    if regist.ValueExists('ECreateDNAEditsLike.Text') then
      ECreateDNAEditsLike.Text := regist.ReadString('ECreateDNAEditsLike.Text');

    if regist.ValueExists('ECreateDNAEditsContLike.Text') then
      ECreateDNAEditsContLike.Text := regist.ReadString('ECreateDNAEditsContLike.Text');

    if regist.ValueExists('ECreateDNAInsertsLike.Text') then
      ECreateDNAInsertsLike.Text := regist.ReadString('ECreateDNAInsertsLike.Text');

    if regist.ValueExists('ECreateDNAInsertsContLike.Text') then
      ECreateDNAInsertsContLike.Text := regist.ReadString('ECreateDNAInsertsContLike.Text');

    if regist.ValueExists('ECreateDNADeletionsLike.Text') then
      ECreateDNADeletionsLike.Text := regist.ReadString('ECreateDNADeletionsLike.Text');

    if regist.ValueExists('ECreateDNADeletionsContLike.Text') then
      ECreateDNADeletionsContLike.Text := regist.ReadString('ECreateDNADeletionsContLike.Text');

    if regist.ValueExists('CBReferenceDNAFormat.Text') then
      CBReferenceDNAFormat.Text := regist.ReadString('CBReferenceDNAFormat.Text');

    if regist.ValueExists('RBReferenceDNAGraphSnips.checked') then
      RBReferenceDNAGraphSnips.Checked := Boolean(StrToInt(regist.ReadString('RBReferenceDNAGraphSnips.checked')));

    if regist.ValueExists('RBReferenceDNAGraphMultiSnips.checked') then
      RBReferenceDNAGraphMultiSnips.Checked := Boolean(StrToInt(regist.ReadString('RBReferenceDNAGraphMultiSnips.checked')));

    if regist.ValueExists('RBReferenceDNAGraphFullMultiSnips.checked') then
      RBReferenceDNAGraphFullMultiSnips.Checked := Boolean(StrToInt(regist.ReadString('RBReferenceDNAGraphFullMultiSnips.checked')));

    if regist.ValueExists('RBReferenceDNAGraphFull.checked') then
      RBReferenceDNAGraphFull.Checked := Boolean(StrToInt(regist.ReadString('RBReferenceDNAGraphFull.checked')));

    if regist.ValueExists('pauseBatchfiles') then
      pauseBatchfiles := Boolean(StrToInt(regist.ReadString('pauseBatchfiles')));

    if regist.ValueExists('CBuseExpSAS.checked') then
      CBuseHashesNew.Checked := Boolean(StrToInt(regist.ReadString('CBuseExpSAS.checked')));

    if regist.ValueExists('CBuseBWT.checked') then
      CBuseBWT.Checked := Boolean(StrToInt(regist.ReadString('CBuseBWT.checked')));

    if regist.ValueExists('CBuseBWTnew.checked') then
      CBuseBWTnew.Checked := Boolean(StrToInt(regist.ReadString('CBuseBWTnew.checked')));

    if regist.ValueExists('CBuseHashes.checked') then
      CBuseHashes.Checked := Boolean(StrToInt(regist.ReadString('CBuseHashes.checked')));

    if regist.ValueExists('CBusePigeonhole.checked') then
      CBusePigeonhole.Checked := Boolean(StrToInt(regist.ReadString('CBusePigeonhole.checked')));

    { empty template for quick adding:

    if regist.ValueExists('') then
    := regist.ReadString('');

    }

  finally
    regist.free;
  end;

  CBReferenceDNAFormatChange(nil);
end;

procedure TFMain.SaveDAB;
var regist: TRegistry;
begin
  regist := TRegistry.Create;
  try
    regist.RootKey := HKEY_CURRENT_USER;
    regist.OpenKey('Software\moyaccercchi\BioInfoGraph\MainCentralControl', true);
    regist.WriteString('fmain.left', InttoStr(FMain.Left));
    regist.WriteString('fmain.top', InttoStr(FMain.Top));
    regist.WriteString('fmain.width', InttoStr(FMain.Width));
    regist.WriteString('fmain.height', InttoStr(FMain.Height));
    regist.WriteString('version', version);
    regist.WriteString('paths.python', PythonPath);
    regist.WriteString('CBReferenceDNAGraph.checked', InttoStr(Integer(CBReferenceDNAGraph.Checked)));
    regist.WriteString('ECreateDNALength.Text', ECreateDNALength.Text);
    regist.WriteString('ECreateDNAAlphabet.Text', ECreateDNAAlphabet.Text);
    regist.WriteString('EPreProcessReferenceHorizon.Text', EPreProcessReferenceHorizon.Text);
    regist.WriteString('ECreateReadsLength.Text', ECreateReadsLength.Text);
    regist.WriteString('ECreateReadsAmount.Text', ECreateReadsAmount.Text);
    regist.WriteString('ECreateReadsMisProb.Text', ECreateReadsMisProb.Text);
    regist.WriteString('EAlignReadsLength.Text', EAlignReadsLength.Text);
    regist.WriteString('EAlignReadsD.Text', EAlignReadsD.Text);
    regist.WriteString('EAlignReadsK.Text', EAlignReadsK.Text);
    regist.WriteString('ECreateDNAEditsLike.Text', ECreateDNAEditsLike.Text);
    regist.WriteString('ECreateDNAEditsContLike.Text', ECreateDNAEditsContLike.Text);
    regist.WriteString('ECreateDNAInsertsLike.Text', ECreateDNAInsertsLike.Text);
    regist.WriteString('ECreateDNAInsertsContLike.Text', ECreateDNAInsertsContLike.Text);
    regist.WriteString('ECreateDNADeletionsLike.Text', ECreateDNADeletionsLike.Text);
    regist.WriteString('ECreateDNADeletionsContLike.Text', ECreateDNADeletionsContLike.Text);
    regist.WriteString('CBReferenceDNAFormat.Text', CBReferenceDNAFormat.Text);
    regist.WriteString('RBReferenceDNAGraphSnips.checked', InttoStr(Integer(RBReferenceDNAGraphSnips.Checked)));
    regist.WriteString('RBReferenceDNAGraphMultiSnips.checked', InttoStr(Integer(RBReferenceDNAGraphMultiSnips.Checked)));
    regist.WriteString('RBReferenceDNAGraphFullMultiSnips.checked', InttoStr(Integer(RBReferenceDNAGraphFullMultiSnips.Checked)));
    regist.WriteString('RBReferenceDNAGraphFull.checked', InttoStr(Integer(RBReferenceDNAGraphFull.Checked)));
    regist.WriteString('pauseBatchfiles', InttoStr(Integer(pauseBatchfiles)));
    regist.WriteString('CBuseExpSAS.checked', InttoStr(Integer(CBuseHashesNew.Checked)));
    regist.WriteString('CBuseHashes.checked', InttoStr(Integer(CBuseHashes.Checked)));
    regist.WriteString('CBuseBWT.checked', InttoStr(Integer(CBuseBWT.Checked)));
    regist.WriteString('CBuseBWTnew.checked', InttoStr(Integer(CBuseBWTnew.Checked)));
    regist.WriteString('CBusePigeonhole.checked', InttoStr(Integer(CBusePigeonhole.Checked)));

    { empty template for quick adding:

    regist.WriteString('', );

    }
  finally
    regist.free;
  end;
end;

procedure TFMain.ResetTo(difficulty: Integer);
begin
  clearEverything;

  case difficulty of
    0:
    begin
      ECreateDNALength.Text := '100';
      ECreateDNAAlphabet.Text := 'ACGT';
      EPreProcessReferenceHorizon.Text := '10';
      ECreateReadsLength.Text := '20';
      ECreateReadsAmount.Text := '20';
      ECreateReadsMisProb.Text := '0';
      EAlignReadsLength.Text := EPreProcessReferenceHorizon.Text;
      EAlignReadsD.Text := '1';
      EAlignReadsK.Text := '2';
      ECreateDNAEditsLike.Text := '50';
      ECreateDNAEditsContLike.Text := '200';
      ECreateDNAInsertsLike.Text := '0';
      ECreateDNAInsertsContLike.Text := '0';
      ECreateDNADeletionsLike.Text := '0';
      ECreateDNADeletionsContLike.Text := '0';
      CBReferenceDNAFormat.Text := 'STPU';
      CBReferenceDNAGraph.Checked := false;
      RBReferenceDNAGraphSnips.Checked := true;
      RBReferenceDNAGraphFull.Checked := false;
      RBReferenceDNAGraphMultiSnips.Checked := false;
      RBReferenceDNAGraphFullMultiSnips.Checked := false;
      CBuseHashesNew.Checked := true;
      CBuseHashes.Checked := false;
      CBuseBWT.Checked := false;
      CBuseBWTnew.Checked := false;
      CBusePigeonhole.Checked := true;        
      MReferenceDNA.Text := 'ATTGGAGGGGTCTCATGCACTAATTATTAAAACGTACAGACCCCCTTCAAAGTCTTCGGCGCGCACATTTTGAAACCTTTGGCGCAGCCAAGCCCTCGTT';
    end;
    1:
    begin
      ECreateDNALength.Text := '1000';
      ECreateDNAAlphabet.Text := 'ACGT';
      EPreProcessReferenceHorizon.Text := '20';
      ECreateReadsLength.Text := '60';
      ECreateReadsAmount.Text := '100';
      ECreateReadsMisProb.Text := '0';
      EAlignReadsLength.Text := EPreProcessReferenceHorizon.Text;
      EAlignReadsD.Text := '2';
      EAlignReadsK.Text := '3';
      ECreateDNAEditsLike.Text := '50';
      ECreateDNAEditsContLike.Text := '200';
      ECreateDNAInsertsLike.Text := '10';
      ECreateDNAInsertsContLike.Text := '0';
      ECreateDNADeletionsLike.Text := '10';
      ECreateDNADeletionsContLike.Text := '0';
      CBReferenceDNAFormat.Text := 'STPU';
      CBReferenceDNAGraph.Checked := false;
      RBReferenceDNAGraphSnips.Checked := true;
      RBReferenceDNAGraphFull.Checked := false;
      RBReferenceDNAGraphMultiSnips.Checked := false;
      RBReferenceDNAGraphFullMultiSnips.Checked := false;
      CBuseHashesNew.Checked := true;
      CBuseHashes.Checked := false;
      CBuseBWT.Checked := false;
      CBuseBWTnew.Checked := false;
      CBusePigeonhole.Checked := true;
      MReferenceDNA.Text := 'GGCTTGTGCTCTTAGATTCCACTTTGGGCACTGATTGACATAAAACTATGGGCTTAGTCTGCTCTCGT' + 'CTCTGACAGCTCCGGCTAACAATGACACACGCTTACTAAAGACGCTGCTCCTAATGTTCAACCGAAACAGGTACAGACACCTGGGCTTTCAGGAAGAAGACTC' +
      'TCTCGGAGGTGCGCGTAACTCTCTATTAATTCCTGCTTCGAAACGCGAGAAGACGCGTCCTATCTCTC' + 'TGCTATAATCAGGTGTGAAGTCGTTAGGGATAGTGTTCCAAAAATATCCCAAAGG' +
      'CATTCGATGAGGTAGCACGAAAAGGCAGAACAGCTTCAAGGGCCTATAGTACACTCGCGCCTCTGACA' + 'AGACGGGGCTGTTAAAGTAGTCTTGGCGGGCACCCCTAAGCATCCGGTCGCGGGTGAAGTATAACCTCCGGAAATACGTCGAC' +
      'GGACGCCGCACTGGCTTACATGGGTTGTTACGCAACGCATCCTTAGACACCGTTATGTGTGACTTTAT' + 'CATTACCGCTTTTGCCCACCGGACGCCTTTACCCCTAGCGACAAGGTACTCCTTCAACGCAGCTCTCTACTCCTGAGCGATGCTACTATCCAAAGAGTCGGGAAGTGCTCTC' +
      'ACATTTTTGAGAAACCTGTATGTCGGATCGCCGACCATGATTGAGTTTACTGACAATGGGGCACCCGA' + 'GCACTCGTTGCGTTGTGGCTTACCACATCTTAAAACCTCTCAGGAAGATACTACTACGGGTTGGATCAGACCTATTGCGGTATATCAACACGGTGCAATTCCGTGGAATCTCAGTGT' +
      'TGAGACCGTGGGCACGATCGAGCTATGAGTGGCTAGGACTGATCAGAGTATCAGGAGCGTATAGGGGA' + 'CACGACGAGCGATGCACAGATTTATCACGGCAACGTGCGACACTTACGCTGCGTCAAGCCGGTTTGGCGGAAATGCAGTATAACCTGAAATGGGCCTGACCCACACTAATACTTAAGGTTGC';
    end;
    2:
    begin
      ECreateDNALength.Text := '2000';
      ECreateDNAAlphabet.Text := 'ACGT';
      EPreProcessReferenceHorizon.Text := '20';
      ECreateReadsLength.Text := '60';
      ECreateReadsAmount.Text := '250';
      ECreateReadsMisProb.Text := '1';
      EAlignReadsLength.Text := EPreProcessReferenceHorizon.Text;
      EAlignReadsD.Text := '2';
      EAlignReadsK.Text := '3';
      ECreateDNAEditsLike.Text := '50';
      ECreateDNAEditsContLike.Text := '200';
      ECreateDNAInsertsLike.Text := '10';
      ECreateDNAInsertsContLike.Text := '200';
      ECreateDNADeletionsLike.Text := '10';
      ECreateDNADeletionsContLike.Text := '200';
      CBReferenceDNAFormat.Text := 'STPU';
      CBReferenceDNAGraph.Checked := false;
      RBReferenceDNAGraphSnips.Checked := true;
      RBReferenceDNAGraphFull.Checked := false;
      RBReferenceDNAGraphMultiSnips.Checked := false;
      RBReferenceDNAGraphFullMultiSnips.Checked := false;
      CBuseHashesNew.Checked := true;
      CBuseHashes.Checked := false;
      CBuseBWT.Checked := false;    
      CBuseBWTnew.Checked := false;
      CBusePigeonhole.Checked := true;
      MReferenceDNA.Text := 'ACCTCTCGATTCACCCTGAATACCTTCCTGGGGGAAAAAATGCGGCCAGACTATCGAATGAAATGTGGACAAGCGCGTCCCACAAATGCCATTTGGCAATCGTTCGTCCCATCA' + 'CAGCATCTGAAAGGTCTCTCGGCCTTTCTCGGAGGTTATCTAACCGGGAGGGCGGGCAGACCCATACTAATGTAGGTGTCTTTAATCACTGATGAGTTTGGTCGAAGAGACGTTTATTGCAAGGTATGTCGGGCAACGGATATTAGGTGTGCCCTCCGTAG' +
      'GTTCATATATAGTTAGGATTCCCGATGAGTCCAAAATTGCTGAAGTGCTGGAACCTAGCCCCTGAAAGGTGCTGCGTGTGTTATGGCCCTGCGACGGGCCTACGGCTTCGTACC' + 'TTTGCCTGCATTCATTCCAACGACACATAGTCTATACCGGTCTATCTCTCTCTTTACACTGAAAAGAGCTCCTTCGCATATTCCAGCTGTGATGAGAGGGAAAGGAACAACGTGAGTAAGTTTGAATCATGTATATAAGGACGGGCGTGATAACGAGT' +
      'GGGGGAGGGTCTTCCTCCAAACATGTGCAATTTTTATGCTTGAGGATGTTTACTTCACTTAGTCCTTACGATTATTGAAGAAGAGTTTGGAATCAGACCACGGTGAAGTTCAGC' + 'AGAATCCCGAGAGCGGCTGGGGCTCAGCACGAAATAACGACTGGCCGCCTGCGGTGAGATGAAGCGGGGCCGGAGGAAAATTAAGACGTCATCGCTTGCAAAAGCGTCATTATAAACGCGTTCTACAAGGGGCGTCTATCGCCCCAAAATTTATGATA' +
      'TTTACCCCGACGTCTGCCCGGCTCTAGGCTTCTGAACAGGCTTCTGACGCGGTATGGTTAAGCTTACCTTCAAACCAATACACTTCAGACGCAGCGATGAGACGTGCGCACGAT' + 'CTACTAACATCGGAGTCCCGATCCTTCCCAAGCCTCAGTGGTTTCCCGGGGCACCCGGTAGAGTTCTAGTCTAATTGCGTGAGTGCTGAAATGGTACAAACTGGGGTATACAGTTAAGCCCGTCAATCGCATTAGGCTCACCCTGCGGCTGAAAGGCATGTT' +
      'CATCCCGCACGTTAGTCGGGGTCACGGTTGATACTCAAAGAATTTGCTGGACCCCGCCTTTGGATTACTGCCTCTCCGCATGGGTACGGACATAACTTCTATTCGAACAAAGGG' + 'CGCTGAATTTGTTGGCGTCAATAGGCAAGTCAAACCATACAGCGACAGAGATAGAATCCAAGTCTAATTAAAATGAGGCGTAATTCGAACGGGGACAGCCACTAGTGGAAGGCAGTCAAATAGGCGAGGTATTCTCGGCTCGCAGTCTATGCCTAGCGACGGTCTCCAA' +
      'AAATGCGACTTCTAGTACCGATCCCGAGTGGGCCGTTCAGGGTTAGCGGGCCAGTAGGATCCTATGTCAGCCTGGCTGTCGATTCTTTATGGTGATTCGGGATAGTCTTAAACC' + 'GAACAGCGCACTCCCCCTGGCGTTGCTTATGTTACCTGGGGCTTCAATCAAGATATCCGTGAAAGTTTACTCAAGAGGGCACGTCCAATAAACTACTGTATGTCCGTGACGCGCATAATCCAAGTCGAGGAGACAGGACGGCTTATTGGCTCGGACGCAGGCCTATG' +
      'TATGGGATAATATAACGCTCAAAGATCTCTGCACTCTAGTGAACTGGGGGCTGGATGCTTAGAGTGAGGTGGCCACTAAATGATGTCAGCCTCCTGACACCACGTTAATGTATC' + 'TGAGTGTTATCAGCCGAAGTGGGCGTATGATATTGGAATACGAGTAAGGGGTACTGCGCTCTCCTCGGACTTAGACCCACGAACGGGGGTACTTCGATGATTGCAGGTCGGAGGTCTCTCGCCGATCTATGCAAGATTTGGACGTCTAGAGAACACGGCTCATGCAACTACCACTTTTCAAACAGGGGTGGCCGCCGTCCATGGTATCAGGGCGAGAATGCTCGCGA';
    end;
    3:
    begin
      ECreateDNALength.Text := '100';
      ECreateDNAAlphabet.Text := 'ACGT';
      EPreProcessReferenceHorizon.Text := '10';
      ECreateReadsLength.Text := '20';
      ECreateReadsAmount.Text := '20';
      ECreateReadsMisProb.Text := '0';
      EAlignReadsLength.Text := EPreProcessReferenceHorizon.Text;
      EAlignReadsD.Text := '1';
      EAlignReadsK.Text := '2';
      ECreateDNAEditsLike.Text := '0';
      ECreateDNAEditsContLike.Text := '0';
      ECreateDNAInsertsLike.Text := '0';
      ECreateDNAInsertsContLike.Text := '0';
      ECreateDNADeletionsLike.Text := '0';
      ECreateDNADeletionsContLike.Text := '0';
      CBReferenceDNAFormat.Text := 'STPU';
      CBReferenceDNAGraph.Checked := true;
      RBReferenceDNAGraphSnips.Checked := false;
      RBReferenceDNAGraphFull.Checked := true;
      RBReferenceDNAGraphMultiSnips.Checked := false;
      RBReferenceDNAGraphFullMultiSnips.Checked := false;
      CBuseHashesNew.Checked := true;
      CBuseHashes.Checked := false;
      CBuseBWT.Checked := false;       
      CBuseBWTnew.Checked := false;
      CBusePigeonhole.Checked := true;
      MReferenceDNA.Text := 'CTCATGCTTAGACCTGCTATTG(C|AT)AACATATGTTTCATGATGAT';
      MCreateDNAres.Text := 'CTCATGCTTAGACCTGCTATTGATAACATATGTTTCATGATGAT';
    end;
  end;

  SaveDAB;
end;

// make everything look pretty on resizing
procedure TFMain.FormResize(Sender: TObject);
var mainleft, gbwidth, gbclientwidth,
    fullwidth,
    halfwidth, halflefttwo,
    thirdwidth, thirdlefttwo, thirdleftthree,
    fourthwidth, fourthlefttwo, fourthleftthree, fourthleftfour: Integer;
begin
  // generate main reference variables

  PMain.Width := SBMain.ClientWidth - 1;

  mainleft := GBCreateDNA.Left;
  gbwidth := PMain.ClientWidth - (2 * mainleft);

  GBCreateDNA.Width := gbwidth;
  GBCreateReads.Width := gbwidth;
  GBReferenceDNA.Width := gbwidth;
  GBPreProcessReference.Width := gbwidth;
  GBAlignReads.Width := gbwidth;
  GBAssembleDNA.Width := gbwidth;
  GBFillGaps.Width := gbwidth;
  GBCallVariants.Width := gbwidth;
  GBAddNotes.Width := gbwidth;

  gbclientwidth := GBCreateDNA.ClientWidth;
  fullwidth := gbclientwidth - (2 * mainleft);
  halfwidth := (gbclientwidth - (3 * mainleft)) div 2;
  halflefttwo := gbclientwidth - (mainleft + halfwidth);
  thirdwidth := (gbclientwidth - (4 * mainleft)) div 3;
  thirdlefttwo := thirdwidth + (2 * mainleft);
  thirdleftthree := gbclientwidth - (mainleft + thirdwidth);
  fourthwidth := (gbclientwidth - (5 * mainleft)) div 4;
  fourthlefttwo := fourthwidth + (2 * mainleft);
  fourthleftthree := (2 * fourthwidth) + (3 * mainleft);
  fourthleftfour := gbclientwidth - (mainleft + fourthwidth);



  // main HUD

  BOptions.Width := (PHUDBtm.Width - (5 * BOptions.Left)) div 4;
  BImportAll.Width := BOptions.Width;
  BExportAll.Width := BOptions.Width;
  BClose.Width := BOptions.Width;
  BImportAll.Left := BOptions.Width + (2 * BOptions.Left);
  BClose.Left := PHUDBtm.Width - (BClose.Width + BOptions.Left);
  BExportAll.Left := BClose.Left - (BExportAll.Width + BOptions.Left);



  // step 1

  LCreateDNAAlphabet.Left := halflefttwo;
  ECreateDNAAlphabet.Left := LCreateDNAAlphabet.Left + LCreateDNAAlphabet.Width + 4;
  ECreateDNAAlphabet.Width := GBCreateDNA.ClientWidth - (BCreateDNACreate.Left + ECreateDNAAlphabet.Left);
  LCreateDNALengthBasepairs.Left := LCreateDNAAlphabet.Left - (LCreateDNALengthBasepairs.Width + LCreateDNALength.Left);
  ECreateDNALength.Left := LCreateDNALength.Width + LCreateDNALength.Left + 4;
  ECreateDNALength.Width := LCreateDNALengthBasepairs.Left - (ECreateDNALength.Left + 4);

  CBReferenceDNAGraph.Width := fourthwidth;
  BReferenceDNACreate.Width := fourthwidth;
  BReferenceDNAImport.Width := fourthwidth;
  BReferenceDNAExport.Width := fourthwidth;    
  BReferenceDNACreate.Left := fourthlefttwo;
  BReferenceDNAImport.Left := fourthleftthree;
  BReferenceDNAExport.Left := fourthleftfour;
  MReferenceDNA.Width := fullwidth;
  CBReferenceDNAFormat.Width := MReferenceDNA.Width + MReferenceDNA.Left - CBReferenceDNAFormat.Left;
  RBReferenceDNAGraphMultiSnips.Left := fourthlefttwo;
  RBReferenceDNAGraphFullMultiSnips.Left := fourthleftthree;
  RBReferenceDNAGraphFull.Left := fourthleftfour;



  // step 2

  BPreProcessReference.Width := fourthwidth;
  BPreProcessReferenceImport.Width := fourthwidth;
  BPreProcessReferenceExport.Width := fourthwidth;
  BPreProcessReference.Left := fourthlefttwo;
  BPreProcessReferenceImport.Left := fourthleftthree;
  BPreProcessReferenceExport.Left := fourthleftfour;
  LPreProcessReferenceHorizonAfter.Left := fourthlefttwo - (LPreProcessReferenceHorizonAfter.Width + MPreProcessReference.Left);
  EPreProcessReferenceHorizon.Left := LPreProcessReferenceHorizonBefore.Left + LPreProcessReferenceHorizonBefore.Width + 4;
  EPreProcessReferenceHorizon.Width := LPreProcessReferenceHorizonAfter.Left - (4 + EPreProcessReferenceHorizon.Left);
  MPreProcessReference.Width := fullwidth;
  MPreProcessReferenceOutRef.Width := fullwidth;
  MPreProcessReferenceBWT.Width := fullwidth;
  MPreProcessReferenceSA.Width := fullwidth;
  BPreProcessReferenceBWT.Width := fullwidth;



  // step 3

  MCreateDNAres.Width := fullwidth;
  BCreateDNAImport.Width := thirdwidth;
  BCreateDNACreate.Width := thirdwidth;
  BCreateDNAExport.Width := thirdwidth;
  BCreateDNAImport.Left := thirdlefttwo;
  BCreateDNAExport.Left := thirdleftthree;

  CBuseHashesNew.Width := fourthwidth;
  CBuseHashes.Width := fourthwidth;
  CBuseBWTnew.Width := fourthwidth;
  CBuseBWT.Width := fourthwidth;
  CBuseHashes.Left := fourthlefttwo;
  CBuseBWTnew.Left := fourthleftthree;
  CBuseBWT.Left := fourthleftfour;

  GBCreateDNAEdits.Width := BCreateDNACreate.Width;
  LCreateDNAEditsLikeAfter.Left := GBCreateDNAEdits.ClientWidth - (LCreateDNAEditsLikeAfter.Width + LCreateDNAEditsLikeBefore.Left);
  ECreateDNAEditsLike.Width := LCreateDNAEditsLikeAfter.Left - (4 + ECreateDNAEditsLike.Left);
  LCreateDNAEditsContLikeAfter.Left := GBCreateDNAEdits.ClientWidth - (LCreateDNAEditsContLikeAfter.Width + LCreateDNAEditsContLikeBefore.Left);
  ECreateDNAEditsContLike.Width := LCreateDNAEditsContLikeAfter.Left - (4 + ECreateDNAEditsContLike.Left);

  GBCreateDNAInserts.Width := BCreateDNAImport.Width;
  GBCreateDNAInserts.Left := BCreateDNAImport.Left;
  LCreateDNAInsertsLikeAfter.Left := GBCreateDNAInserts.ClientWidth - (LCreateDNAInsertsLikeAfter.Width + LCreateDNAInsertsLikeBefore.Left);
  ECreateDNAInsertsLike.Width := LCreateDNAInsertsLikeAfter.Left - (4 + ECreateDNAInsertsLike.Left);
  LCreateDNAInsertsContLikeAfter.Left := GBCreateDNAInserts.ClientWidth - (LCreateDNAInsertsContLikeAfter.Width + LCreateDNAInsertsContLikeBefore.Left);
  ECreateDNAInsertsContLike.Width := LCreateDNAInsertsContLikeAfter.Left - (4 + ECreateDNAInsertsContLike.Left);

  GBCreateDNADeletions.Width := BCreateDNAExport.Width;
  GBCreateDNADeletions.Left := BCreateDNAExport.Left;
  LCreateDNADeletionsLikeAfter.Left := GBCreateDNADeletions.ClientWidth - (LCreateDNADeletionsLikeAfter.Width + LCreateDNADeletionsLikeBefore.Left);
  ECreateDNADeletionsLike.Width := LCreateDNADeletionsLikeAfter.Left - (4 + ECreateDNADeletionsLike.Left);
  LCreateDNADeletionsContLikeAfter.Left := GBCreateDNADeletions.ClientWidth - (LCreateDNADeletionsContLikeAfter.Width + LCreateDNADeletionsContLikeBefore.Left);
  ECreateDNADeletionsContLike.Width := LCreateDNADeletionsContLikeAfter.Left - (4 + ECreateDNADeletionsContLike.Left);



  // step 4

  LCreateReadsAmountBefore.Left := (GBCreateReads.ClientWidth div 3) + (LCreateReadsLengthBefore.Left div 2);
  LCreateReadsMisProbBefore.Left := (2 * GBCreateReads.ClientWidth div 3) + (LCreateReadsLengthBefore.Left div 2);
  LCreateReadsMisProbAfter.Left := GBCreateReads.ClientWidth - (LCreateReadsMisProbAfter.Width + LCreateReadsLengthBefore.Left);
  ECreateReadsMisProb.Left := LCreateReadsMisProbBefore.Left + LCreateReadsMisProbBefore.Width + 4;
  ECreateReadsMisProb.Width := LCreateReadsMisProbAfter.Left - (ECreateReadsMisProb.Left + 4);
  ECreateReadsAmount.Left := LCreateReadsAmountBefore.Left + LCreateReadsAmountBefore.Width + 4;
  ECreateReadsAmount.Width := LCreateReadsMisProbBefore.Left - (ECreateReadsAmount.Left + LCreateReadsLengthBefore.Left + 4);
  LCreateReadsLengthAfter.Left := LCreateReadsAmountBefore.Left - (LCreateReadsLengthAfter.Width + LCreateReadsLengthBefore.Left);
  ECreateReadsLength.Left := LCreateReadsLengthBefore.Left + LCreateReadsLengthBefore.Width + 4;
  ECreateReadsLength.Width := LCreateReadsLengthAfter.Left - (ECreateReadsLength.Left + 4);

  MCreateReadsres.Width := halfwidth;
  MCreateReadspos.Width := halfwidth;
  MCreateReadspos.Left := halflefttwo;
  LCreateReadsMpos.Left := halflefttwo;

  BCreateReadsImport.Width := thirdwidth;
  BCreateReadsCreate.Width := thirdwidth;
  BCreateReadsExport.Width := thirdwidth;
  BCreateReadsImport.Left := thirdlefttwo;
  BCreateReadsExport.Left := thirdleftthree;



  // step 5

  BAlignReads.Width := thirdwidth;
  BAlignReadsImport.Width := thirdwidth;
  BAlignReadsExport.Width := thirdwidth;
  BAlignReadsImport.Left := thirdlefttwo;
  BAlignReadsExport.Left := thirdleftthree;
  MAlignReads.Width := fullwidth;
  LAlignReadsDBefore.Left := BAlignReadsExport.Left;
  EAlignReadsD.Left := LAlignReadsDBefore.Left + LAlignReadsDBefore.Width + 4;
  LAlignReadsDAfter.Left := GBAlignReads.Width - (BAlignReads.Left + LAlignReadsDAfter.Width);
  EAlignReadsD.Width := (LAlignReadsDAfter.Left - (12 + EAlignReadsD.Left + LAlignReadsDMiddle.Width)) div 2;
  EAlignReadsK.Width := EAlignReadsD.Width;
  LAlignReadsDMiddle.Left := EAlignReadsD.Left + EAlignReadsD.Width + 4;
  EAlignReadsK.Left := LAlignReadsDMiddle.Left + LAlignReadsDMiddle.Width + 4;
  EAlignReadsLength.Left := LAlignReadsLengthBefore.Left + LAlignReadsLengthBefore.Width + 4;
  LAlignReadsLengthAfter.Left := BAlignReads.Width + BAlignReads.Left - LAlignReadsDAfter.Width;
  EAlignReadsLength.Width := LAlignReadsLengthAfter.Left - (4 + EAlignReadsLength.Left);



  // step 6

  BAssembleDNA.Width := thirdwidth;
  BAssembleDNAImport.Width := thirdwidth;
  BAssembleDNAExport.Width := thirdwidth;
  BAssembleDNAImport.Left := thirdlefttwo;
  BAssembleDNAExport.Left := thirdleftthree;
  MAssembleDNA.Width := fullwidth;
  BAssembleDNAShowAlign.Width := fullwidth;
  MAssembleDNAShowAlign.Width := fullwidth;



  // step 7

  BFillGaps.Width := thirdwidth;
  BFillGapsImport.Width := thirdwidth;
  BFillGapsExport.Width := thirdwidth;
  BFillGapsImport.Left := thirdlefttwo;
  BFillGapsExport.Left := thirdleftthree;
  MFillGaps.Width := fullwidth;



  // step 8

  BCallVariants.Width := thirdwidth;
  BCallVariantsImport.Width := thirdwidth;
  BCallVariantsExport.Width := thirdwidth;
  BCallVariantsImport.Left := thirdlefttwo;
  BCallVariantsExport.Left := thirdleftthree;

  LCallVariantsFRD.Width := thirdwidth;
  LCallVariantsRAD.Width := thirdwidth;
  LCallVariantsOV.Width := thirdwidth;
  LCallVariantsRAD.Left := thirdlefttwo;
  LCallVariantsOV.Left := thirdleftthree;

  MCallVariantsFRD.Width := thirdwidth;
  MCallVariantsRAD.Width := thirdwidth;
  MCallVariantsOV.Width := thirdwidth;
  MCallVariantsRAD.Left := thirdlefttwo;
  MCallVariantsOV.Left := thirdleftthree;



  // step 9

  MAddNotes.Width := fullwidth;



  // rest of the HUD

  SBAllMemos.Left := MAssembleDNA.Left + GBAssembleDNA.Left;
  SBAllMemos.Width := fullwidth;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  FormResize(nil);
end;

procedure TFMain.BCloseClick(Sender: TObject);
begin
  SaveDAB;
  FMain.Close;
end;

procedure TFMain.BCreateDNACreateClick(Sender: TObject);
var reference, res, alphabet, outtext, refbeg, refmid, refend: string;
    len, alen, i, edits, insertions, deletions,
    dellike, delcontlike, inslike, inscontlike, edtlike, edtcontlike,
    del_al, ins_al, edt_al, openedPars: Integer;
    del_start, del_end, ins_start, ins_end, edt_start, edt_end: array of Integer;
    nextChar: Char;
    refmidsl: TStringList;
begin
  del_al := 0;
  ins_al := 0;
  edt_al := 0;
  SetLength(del_start, 100);
  SetLength(del_end, 100);
  SetLength(ins_start, 100);
  SetLength(ins_end, 100);
  SetLength(edt_start, 100);
  SetLength(edt_end, 100);

  res := '';
  if (MReferenceDNA.Text = '') or (MReferenceDNA.Text = #13#10) then
    begin
      if Sender = nil then
        ShowMessage('No reference string can be created!')
      else
        begin
          BReferenceDNACreateClick(Sender);
          BCreateDNACreateClick(nil);
        end;
      exit;                  
    end;

  if CBReferenceDNAFormat.Text = 'FASTA' then
    begin
      reference := '';
      i := 1;
      while i < MReferenceDNA.Lines.Count do
        begin
          reference := reference + MReferenceDNA.Lines[i];
          Inc(i);
        end;
    end
  else
    begin
      // STPU

      reference := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);

      // choose a random path through the graph

      {
      // This version only works for two-SNIPs; not for several-SNIPs,
      // and definitely not for general graphs!
      while pos('(', reference) > 0 do
      begin
      refbeg := copy(reference, 1, pos('(', reference) - 1);
      refmid := copy(reference, pos('(', reference) + 1, pos(')', reference) - (1 + pos('(', reference)));
      refend := copy(reference, pos(')', reference) + 1, Length(reference));

      if Random(2) = 0 then
      refmid := copy(refmid, 1, pos('|', refmid) - 1)
      else
      refmid := copy(refmid, pos('|', refmid) + 1, Length(refmid));

      reference := refbeg + refmid + refend;
      end;
      }

      {
      refmidsl := TStringList.Create;

      // This version works for many-SNIPs.
      // However, nested graphs are still a no-no.
      while pos('(', reference) > 0 do
      begin
      refbeg := copy(reference, 1, pos('(', reference) - 1);
      refmid := copy(reference, pos('(', reference) + 1, pos(')', reference) - (1 + pos('(', reference))) + '|';
      refend := copy(reference, pos(')', reference) + 1, Length(reference));

      refmidsl.Text := '';

      while pos('|', refmid) > 0 do
      begin
      refmidsl.Add(copy(refmid, 1, pos('|', refmid) - 1));
      refmid := copy(refmid, pos('|', refmid) + 1, Length(refmid));
      end;

      refmid := refmidsl.Strings[Random(refmidsl.Count)];

      reference := refbeg + refmid + refend;
      end;

      refmidsl.Free;
      }

      refmidsl := TStringList.Create;

      // This version works for many-SNIPs AND nested graphs.
      // The magic ingredient is to continue going for the first '(',
      // but not pair it with the first ')' - instead, pair it with the
      // first ')' that is on its own level. E.g. do not take ((), but (()).
      while pos('(', reference) > 0 do
        begin
          refbeg := copy(reference, 1, pos('(', reference) - 1);
          openedPars := 1;
          i := pos('(', reference);
          repeat
            Inc(i);
            if reference[i] = ')' then
              Dec(openedPars)
            else
              if reference[i] = '(' then
                Inc(openedPars)
              else
                if (reference[i] = '|') and (openedPars > 1) then // prevent refmid-selector from breaking at this breakpoint (as it is contained in a nested graph)
                  reference[i] := '%';
          until openedPars < 1;
          refmid := copy(reference, pos('(', reference) + 1, i - (1 + pos('(', reference))) + '|';
          refend := copy(reference, i + 1, Length(reference));

          refmidsl.Text := '';

          while pos('|', refmid) > 0 do
            begin
              refmidsl.Add(copy(refmid, 1, pos('|', refmid) - 1));
              refmid := copy(refmid, pos('|', refmid) + 1, Length(refmid));
            end;

          refmid := refmidsl.Strings[Random(refmidsl.Count)];

          reference := refbeg + StringReplace(refmid, '%', '|', [rfReplaceall]) + refend;
        end;

      refmidsl.Free;
    end;

  len := Length(reference);
  alphabet := ECreateDNAAlphabet.Text;
  if (Length(alphabet) = 0) then
    begin
      ShowMessage('Please specify an alphabet first.');
      exit;
    end;
  alen := Length(alphabet);

  i := 0;
  edits := 0;
  insertions := 0;
  deletions := 0;

  edtlike := StrtoInt(ECreateDNAEditsLike.Text);
  edtcontlike := StrtoInt(ECreateDNAEditsContLike.Text);
  inslike := StrtoInt(ECreateDNAInsertsLike.Text);
  inscontlike := StrtoInt(ECreateDNAInsertsContLike.Text);
  dellike := StrtoInt(ECreateDNADeletionsLike.Text);
  delcontlike := StrtoInt(ECreateDNADeletionsContLike.Text);

  while i < len do
    begin
      // deletions
      if Random(1000) < dellike then
        begin
          repeat
            Inc(i);
            Inc(deletions);
          until not ((Random(1000) < delcontlike) and (i < len - 1));

          while del_al > High(del_start) do
            begin
              SetLength(del_start, 2 * High(del_start));
              SetLength(del_end, 2 * High(del_end));
            end;

          del_start[del_al] := Length(res) - 1;
          del_end[del_al] := Length(res) + 1;
          Inc(del_al);
        end;

      // insertions
      if Random(1000) < inslike then
        begin
          while ins_al > High(ins_start) do
            begin
              SetLength(ins_start, 2 * High(ins_start));
              SetLength(ins_end, 2 * High(ins_end));
            end;

          ins_start[ins_al] := Length(res);

          repeat
            Inc(insertions);
            res := res + alphabet[Random(alen)+1];
          until not (Random(1000) < inscontlike);

          ins_end[ins_al] := Length(res);
          Inc(ins_al);
        end;

      // edits
      if Random(1000) < edtlike then
        begin
          while edt_al > High(edt_start) do
            begin
              SetLength(edt_start, 2 * High(edt_start));
              SetLength(edt_end, 2 * High(edt_end));
            end;

          edt_start[edt_al] := Length(res);

          repeat
            Inc(i);
            Inc(edits);
            repeat
              nextChar := alphabet[Random(alen)+1];
            until not (nextChar = reference[i]);
            res := res + nextChar;
          until not ((Random(1000) < edtcontlike) and (i < len - 1));

          edt_end[edt_al] := Length(res);
          Inc(edt_al);
        end
      else
        // no change
        begin
          Inc(i);
          res := res + reference[i];
        end;
    end;

  if (CBReferenceDNAFormat.Text = 'FASTA') then
    begin
      // add comment line in the beginning
      outtext := '>IND - individual DNA string, MCC v ' + version + ' by Moyaccercchi';

      // split sequence up into lines, each 70 characters long
      len := Length(res);
      i := 1;
      while i < len do
        begin
          outtext := outtext + #13#10 + copy(res, i, 70);
          i := i + 70;
        end;

      res := outtext;
    end
  else
    if (CBReferenceDNAFormat.Text = 'FASTG') then
      begin
        ShowMessage('Sorry, FASTG generation is not yet possible.');
      end
    else
      if (CBReferenceDNAFormat.Text = 'GFA') then
        begin
          ShowMessage('Sorry, GFA generation is not yet possible.');
        end;

  MCreateDNAres.Text := res;
  MCreateDNAres.SelectAll;
  MCreateDNAres.SelAttributes.Color := MCreateDNAres.Font.Color;
  MCreateDNAres.SelAttributes.Style := [];

  // color me interested
  if CBReferenceDNAFormat.Text = 'STPU' then
    begin
      for i := 0 to del_al do
        begin
          MCreateDNAres.SelStart := del_start[i];
          MCreateDNAres.SelLength := del_end[i] - del_start[i];
          MCreateDNAres.SelAttributes.Style := [fsUnderline];
        end;

      for i := 0 to ins_al do
        begin
          MCreateDNAres.SelStart := ins_start[i];
          MCreateDNAres.SelLength := ins_end[i] - ins_start[i];
          MCreateDNAres.SelAttributes.Color := clBlue;
        end;

      for i := 0 to edt_al do
        begin
          MCreateDNAres.SelStart := edt_start[i];
          MCreateDNAres.SelLength := edt_end[i] - edt_start[i];
          MCreateDNAres.SelAttributes.Color := clFuchsia;
        end;
    end;

  LCreateDNAStats.Caption := 'Status: Individual DNA string with ' + InttoStr(edits * 1000 div len) + '‰ changes, ' + InttoStr(insertions * 1000 div len) + '‰ insertions and ' + InttoStr(deletions * 1000 div len) + '‰ deletions with regards to reference string';
end;

// regular reference string generation, used by graphless STPU
function TFMain.generateSTPUstringReference(len, alen: Integer; alphabet: string): string;
var i: Integer;
begin
  result := '';

  for i := 1 to len do
    result := result + alphabet[Random(alen)+1];
end;

// STPU reference graph generation
function TFMain.generateSTPUgraphReference(len, alen: Integer; alphabet: string): string;
begin
  if RBReferenceDNAGraphSnips.Checked then
    result := generateSTPUgraphReference_S(len, alen, alphabet)
  else
    if RBReferenceDNAGraphMultiSnips.Checked then
      result := generateSTPUgraphReference_M(len, alen, alphabet)
    else
      if RBReferenceDNAGraphFullMultiSnips.Checked then
        result := generateSTPUgraphReference_FM(len, alen, alphabet, 98)
      else
        result := generateSTPUgraphReference_F(len, alen, alphabet);

  while pos('()', result) > 0 do
    result := stringreplace(result, '()', '', [rfReplaceAll]);
end;

function TFMain.generateSTPUgraphReference_S(len, alen: Integer; alphabet: string): string;
var i, k: Integer;
    newstr: string;
begin
  result := '';

  i := 0;

  while i < len do
    begin
      if (Random(100) < 98) then
        begin
          result := result + alphabet[Random(alen)+1];
          Inc(i);
        end
      else
        begin
          result := result + '(';

          newstr := alphabet[Random(alen)+1] + '|';
          repeat
            k := Random(alen)+1;
          until pos(alphabet[k], newstr) = 0;
          newstr := newstr + alphabet[k];

          result := result + newstr + ')';
          Inc(i);
        end;
    end;
end;

function TFMain.generateSTPUgraphReference_M(len, alen: Integer; alphabet: string): string;
var windowlen, i, preventInfLoopsI: Integer;
    newstr, possiblestr: string;
    const
    preventInfLoopsMax = 1000;
begin
  result := '';

  i := 0;

  while i < len do
    begin
      if (Random(100) < 98) then
        begin
          result := result + alphabet[Random(alen)+1];
          Inc(i);
        end
      else
        begin
          windowlen := Random(min(10, len-i));
          result := result + '(';
          newstr := generateSTPUstringReference(windowlen, alen, alphabet) + '|';
          repeat
            preventInfLoopsI := 0;
            repeat
              possiblestr := generateSTPUstringReference(windowlen, alen, alphabet) + '|';
              Inc(preventInfLoopsI);
            until (pos('|' + possiblestr, '|' + newstr) = 0) or (preventInfLoopsI > preventInfLoopsMax);
            if preventInfLoopsI <= preventInfLoopsMax then
              newstr := newstr + possiblestr;
          until Random(10) < 9;

          // take off the trailing |
          newstr := copy(newstr, 1, Length(newstr) - 1);

          result := result + newstr + ')';
          i := i + windowlen;
        end;
    end;
end;

function TFMain.generateSTPUgraphReference_FM(len, alen: Integer; alphabet: string; P_graphless: Integer): string;
var windowlen, i, preventInfLoopsI: Integer;
    newstr, possiblestr: string;
    const
    preventInfLoopsMax = 1000;
begin
  result := '';

  i := 0;

  while i < len do
    begin
      if (Random(100) < P_graphless) then
        begin
          result := result + alphabet[Random(alen)+1];
          Inc(i);
        end
      else
        begin
          windowlen := Random(min(10, len-i));
          result := result + '(';
          newstr := generateSTPUgraphReference_FM(windowlen, alen, alphabet, 70) + '|';
          repeat
            preventInfLoopsI := 0;
            repeat
              possiblestr := generateSTPUgraphReference_FM(windowlen, alen, alphabet, 70) + '|';
              Inc(preventInfLoopsI);
            until (pos('|' + possiblestr, '|' + newstr) = 0) or (preventInfLoopsI > preventInfLoopsMax);
            if preventInfLoopsI <= preventInfLoopsMax then
              newstr := newstr + possiblestr;
          until Random(10) < 9;

          // take off the trailing |
          newstr := copy(newstr, 1, Length(newstr) - 1);

          result := result + newstr + ')';
          i := i + windowlen;
        end;
    end;
end;

function TFMain.generateSTPUgraphReference_F(len, alen: Integer; alphabet: string): string;
var windowlen, i, preventInfLoopsI: Integer;
    startwithtwo: Boolean;
    newstr, possiblestr: string;
    const
    preventInfLoopsMax = 1000;
begin
  result := '';

  i := 0;

  while i < len do
    begin
      if (Random(100) < 98) then
        begin
          result := result + alphabet[Random(alen)+1];
          Inc(i);
        end
      else
        begin
          windowlen := Random(10);
          result := result + '(';
          newstr := '';

          // 50-50 for getting two to start with or one to start with
          startwithtwo := Random(10) < 6;
          if startwithtwo then
            newstr := generateSTPUgraphReference_F(windowlen + Random(5), alen, alphabet) + '|';
          repeat
            preventInfLoopsI := 0;
            repeat
              possiblestr := generateSTPUgraphReference_F(windowlen + Random(5), alen, alphabet) + '|';
              Inc(preventInfLoopsI);
            until (pos('|' + possiblestr, '|' + newstr) = 0) or (preventInfLoopsI > preventInfLoopsMax);
            if preventInfLoopsI <= preventInfLoopsMax then
              newstr := newstr + possiblestr;
          until Random(10) < 9;

          // sometimes take off the trailing |, but not always
          if startwithtwo and (Random(10) < 8) then
            newstr := copy(newstr, 1, Length(newstr) - 1);

          result := result + newstr + ')';
          i := i + windowlen + 2;
        end;
    end;
end;

// reference string generation, used by FASTA
function TFMain.generateFASTAstringReference(len, alen: Integer; alphabet: string): string;
var i: Integer;
    outtext: string;
begin
  // add comment line in the beginning
  outtext := '>RAND - random reference sequence, MCC v ' + version + ' by Moyaccercchi';

  // regular reference string generation, used by FASTA
  for i := 1 to len do
    result := result + alphabet[Random(alen)+1];

  // split sequence up into lines, each 70 characters long
  len := Length(result);
  i := 1;
  while i < len do
    begin
      outtext := outtext + #13#10 + copy(result, i, 70);
      i := i + 70;
    end;

  result := outtext;
end;

// FASTG generation without a graph - a bit pointless; but only a bit! =)
function TFMain.generateFASTGstringReference(len, alen: Integer; alphabet: string): string;
var i: Integer;
begin
  result := '#FASTG:begin;' + #13#10 +
  '#FASTG:version=1.0:assembly_name="RAND - random reference sequence";' + #13#10;

  result := result + '>chr1;' + #13#10;

  for i := 1 to len do
    result := result + alphabet[Random(alen)+1];

  result := result + #13#10 + '#FASTG:end;'
end;

// FASTG generation with a graph - we here only use some of the many FASTG features,
// as FASTG has a looot of features and quite a lot of them are pretty unnecessary for us right now
function TFMain.generateFASTGgraphReference(len, alen: Integer; alphabet: string): string;
begin
  if RBReferenceDNAGraphSnips.Checked then
    result := generateFASTGgraphReference_S(len, alen, alphabet)
  else
    if RBReferenceDNAGraphMultiSnips.Checked then
      result := generateFASTGgraphReference_M(len, alen, alphabet)
    else
      result := generateFASTGgraphReference_F(len, alen, alphabet);
end;

function TFMain.generateFASTGgraphReference_S(len, alen: Integer; alphabet: string): string;
var i: Integer;
    randstr, randstr2, randstr3: string;
begin
  result := '#FASTG:begin;' + #13#10 +
  '#FASTG:version=1.0:assembly_name="RAND - random reference sequence";' + #13#10;

  // example file:
  // >chr1:chr1;
  // ACGANNNNN[5:gap:size=(5,4..6)]CAGGC[1:alt:allele|C,G]TATACG
  // >chr2;4
  // ACATACGCATATATATATATATATATAT[20:tandem:size=(10,8..12)|AT]TCAGGCA[1:alt|A,T,TT]GGAC

  result := result + '>chr1;' + #13#10;

  for i := 1 to len do
    begin
      if (Random(100) < 10) then
        begin
          // alt:allele
          randstr := alphabet[Random(alen)+1];
          randstr2 := '';
          // repeat // take out to only create 2-alternative-snips
          repeat
            randstr3 := ',' + alphabet[Random(alen)+1];
          until pos(randstr3 + ',', ',' + randstr + randstr2 + ',') < 1;
          randstr2 := randstr2 + randstr3;
          // until (Random(10) < 6) or (Length(randstr + randstr2) > 6); // take out to only create 2-alternative-snips
          result := result + randstr + '[1:alt:allele|' + randstr + randstr2 + ']';
        end
      else
        begin
          // just a character
          result := result + alphabet[Random(alen)+1];
        end;
    end;

  result := result + #13#10 + '#FASTG:end;'
end;

function TFMain.generateFASTGgraphReference_M(len, alen: Integer; alphabet: string): string;
var i: Integer;
    randstr, randstr2, randstr3: string;
begin
  showMessage('Sorry, not yet available');

  result := '#FASTG:begin;' + #13#10 +
  '#FASTG:version=1.0:assembly_name="RAND - random reference sequence";' + #13#10;

  // example file:
  // >chr1:chr1;
  // ACGANNNNN[5:gap:size=(5,4..6)]CAGGC[1:alt:allele|C,G]TATACG
  // >chr2;4
  // ACATACGCATATATATATATATATATAT[20:tandem:size=(10,8..12)|AT]TCAGGCA[1:alt|A,T,TT]GGAC

  result := result + '>chr1;' + #13#10;

  for i := 1 to len do
    begin
      if (Random(100) < 10) then
        begin
          // alt:allele
          randstr := alphabet[Random(alen)+1];
          randstr2 := '';
          // repeat // take out to only create 2-alternative-snips
          repeat
            randstr3 := ',' + alphabet[Random(alen)+1];
          until pos(randstr3 + ',', ',' + randstr + randstr2 + ',') < 1;
          randstr2 := randstr2 + randstr3;
          // until (Random(10) < 6) or (Length(randstr + randstr2) > 6); // take out to only create 2-alternative-snips
          result := result + randstr + '[1:alt:allele|' + randstr + randstr2 + ']';
        end
      else
        begin
          // just a character
          result := result + alphabet[Random(alen)+1];
        end;
    end;

  result := result + #13#10 + '#FASTG:end;'
end;

function TFMain.generateFASTGgraphReference_F(len, alen: Integer; alphabet: string): string;
var i, j, counter: Integer;
    randstr, randstr2, randstr3: string;
begin
  result := '#FASTG:begin;' + #13#10 +
  '#FASTG:version=1.0:assembly_name="RAND - random reference sequence";' + #13#10;

  // example file:
  // >chr1:chr1;
  // ACGANNNNN[5:gap:size=(5,4..6)]CAGGC[1:alt:allele|C,G]TATACG
  // >chr2;4
  // ACATACGCATATATATATATATATATAT[20:tandem:size=(10,8..12)|AT]TCAGGCA[1:alt|A,T,TT]GGAC

  result := result + '>chr1;' + #13#10;

  for i := 1 to len do
    begin
      if (Random(100) < 10) then
        begin
          // alt:allele
          randstr := alphabet[Random(alen)+1];
          randstr2 := '';
          repeat
            repeat
              randstr3 := ',' + alphabet[Random(alen)+1];
            until pos(randstr3 + ',', ',' + randstr + randstr2 + ',') < 1;
            randstr2 := randstr2 + randstr3;
          until (Random(10) < 6) or (Length(randstr + randstr2) > 6);
          result := result + randstr + '[1:alt:allele|' + randstr + randstr2 + ']';
        end
      else
        if (Random(100) < 5) then
          begin
            // alt
            randstr := '';
            for j := 1 to Random(5)+2 do
              randstr := randstr + alphabet[Random(alen)+1];
            randstr2 := '';
            counter := 0;
            repeat
              repeat
                randstr3 := ',';
                for j := 1 to Random(5)+2 do
                  randstr3 := randstr3 + alphabet[Random(alen)+1];
              until pos(randstr3 + ',', ',' + randstr + randstr2 + ',') < 1;
              randstr2 := randstr2 + randstr3;
              inc(counter);
            until (Random(10) < 6) or (counter > 100);
            result := result + randstr + '[' + InttoStr(Length(randstr)) + ':alt|' + randstr + randstr2 + ']';
          end
        else
          begin
            // just a character
            result := result + alphabet[Random(alen)+1];
          end;
    end;

  result := result + #13#10 + '#FASTG:end;'
end;

// GFA generation without a graph
function TFMain.generateGFAstringReference(len, alen: Integer; alphabet: string): string;
var i: Integer;
begin
  result := 'H' + #9 + 'VN:Z:1.0';

  result := result + #13#10 + 'S' + #9 + '1' + #9;

  for i := 1 to len do
    result := result + alphabet[Random(alen)+1];
end;

// GFA graph reference generation
function TFMain.generateGFAgraphReference(len, alen: Integer; alphabet: string): string;
begin
  if RBReferenceDNAGraphSnips.Checked then
    result := generateGFAgraphReference_S(len, alen, alphabet)
  else
    if RBReferenceDNAGraphMultiSnips.Checked then
      result := generateGFAgraphReference_M(len, alen, alphabet)
    else
      result := generateGFAgraphReference_F(len, alen, alphabet);
end;

function TFMain.generateGFAgraphReference_S(len, alen: Integer; alphabet: string): string;
begin
  ShowMessage('Sorry, snips-only graph reference generation in GFA is not yet available.');
end;

function TFMain.generateGFAgraphReference_M(len, alen: Integer; alphabet: string): string;
begin
  ShowMessage('Sorry, multi-snips-only graph reference generation in GFA is not yet available.');
end;

function TFMain.generateGFAgraphReference_F(len, alen: Integer; alphabet: string): string;
var i, curSeqAmount: Integer;
    linklines, newlink: string;
begin
  result := 'H' + #9 + 'VN:Z:1.0';

  result := result + #13#10 + 'S' + #9 + '1' + #9;
  curSeqAmount := 2;

  for i := 1 to len do
    begin
      if (not (result[Length(result)] = #9)) and (Random(100) < 10) then
        begin
          // create a new link to join the new sequence to the rest
          // (here it is actually simpler to create the links before the sequences,
          // which is proposed in the GFA standard (but the community doesn't link it)
          // so we will do that, but we could also via an optional parameter or so
          // enforce that the links follow the sequences, which could easily be done
          // by writing the linklines out before writing the next sequence line,
          // and in the end)
          linklines := '';
          repeat
            // so far, all links are ++ links with the 0M (no overlap) CIGAR,
            // which is not very flexible, but should be enough for now
            newlink := #13#10 + 'L' + #9 + InttoStr(Random(curSeqAmount-1)+1) + #9 + '+' + #9 + InttoStr(curSeqAmount) + #9 + '+' + #9 + '0M';
            if (pos(newlink, linklines) = 0) then
              linklines := linklines + newlink;
          until Random(100) < 50;
          result := result + linklines;

          // create the new sequence
          result := result + #13#10 + 'S' + #9 + InttoStr(curSeqAmount) + #9;
          Inc(curSeqAmount);
        end;

      // add a character to the current sequence
      result := result + alphabet[Random(alen)+1];
    end;
end;

function TFMain.createRandomDNAString: string;
var len, alen: Integer;
    alphabet: string;
begin
  result := '';

  len := StrtoInt(ECreateDNALength.Text);
  alphabet := ECreateDNAAlphabet.Text;
  alen := Length(alphabet);
  if (alen = 0) then
    begin
      ShowMessage('Please specify an alphabet first.');
      exit;
    end;

  if (CBReferenceDNAFormat.Text = 'STPU') then
    begin
      if (CBReferenceDNAGraph.Checked) then
        result := generateSTPUgraphReference(len, alen, alphabet)
      else
        result := generateSTPUstringReference(len, alen, alphabet);
    end
  else
    if (CBReferenceDNAFormat.Text = 'FASTA') then
      begin
        if (CBReferenceDNAGraph.Checked) then
          ShowMessage('In FASTA, a graph reference cannot be created.')
        else
          result := generateFASTAstringReference(len, alen, alphabet);
      end
    else
      if (CBReferenceDNAFormat.Text = 'FASTG') then
        begin
          if (CBReferenceDNAGraph.Checked) then
            result := generateFASTGgraphReference(len, alen, alphabet)
          else
            result := generateFASTGstringReference(len, alen, alphabet);
        end
      else
        if (CBReferenceDNAFormat.Text = 'GFA') then
          begin
            if (CBReferenceDNAGraph.Checked) then
              result := generateGFAgraphReference(len, alen, alphabet)
            else
              result := generateGFAstringReference(len, alen, alphabet);
          end;
end;

procedure TFMain.FormCreate(Sender: TObject);
const hint_dplusk = 'd+k where d is the main maximum amount and d+k is used for less intensive parts' + #13#10 + 'of the algorithm to achieve more matches without inflating the costs too much';
begin
  Randomize;                          

  // initialize before loading DAB...
  pauseBatchfiles := false;

  Path := copyend(Application.Exename, '\', 1) + '\';
  MainPath := copyend(Application.Exename, '\', 3) + '\';
  ODGeneral.InitialDir := MainPath;
  SDGeneral.InitialDir := MainPath;
  LoadDAB;

  LVersion.Caption := 'Version: ' + version[1] + '.' + version[2] + '.' + version[3] + '.' + version[4] + ' by Moyaccercchi, 7. 10. 2014 .. ' + verdate;

  LAlignReadsDBefore.Hint := hint_dplusk;
  EAlignReadsD.Hint := hint_dplusk;
  LAlignReadsDMiddle.Hint := hint_dplusk;
  EAlignReadsK.Hint := hint_dplusk;
  LAlignReadsDAfter.Hint := hint_dplusk;

  SBMain.VertScrollBar.Position := 0;
  MCreateReadsres_reacttoonchange := true;
  MAssembleDNA_reacttoonchange := true;
  MAlignReads_reacttoonchange := true;
  MPreProcessReference_reacttoonchange := true;
  lastMAssembleDNAtext := '';
  lastMFillGapstext := '';
  AssembleDNA_quality := 'nil';
  showPreProcessReferenceOutRef := false;
end;

function TFMain.copyend(str, substr: string; depth: Integer): string;
// in:  'abcdefghijsubstrklmnopqrstuvwxyz' and 'substr'
// out: 'abcdefghij'
var vari, varlen: Integer;
begin
  if (pos(substr, str) = 0) or (substr + str = '') then
    result := str
  else
    if Length(substr) = 1 then
      begin
        vari := Length(str);
        while depth > 0 do
          begin
            while not (str[vari] = substr) do
              Dec(vari);
            Dec(depth);
            Dec(vari);
          end;
        result := copy(str, 1, vari);
      end
    else
      begin
        varlen := Length(substr);
        vari := Length(str) + 1 - varlen;
        while depth > 0 do
          begin
            while not (copy(str, vari, varlen) = substr) do
              Dec(vari);
            Dec(depth);
            Dec(vari);
          end;
        result := copy(str, 1, vari);
      end;
end;

procedure TFMain.BCreateReadsCreateClick(Sender: TObject);
var lengthstring, amountstring, misprobstring, alphabetstring: string;
    varsl: TStringList;
begin
  if python_path_set then
    begin
      MCreateReadsres.Text := '';
      MCreateReadspos.Text := '';

      if (MCreateDNAres.Text = '') or (MCreateDNAres.Text = #13#10) then
        begin
          if (Sender = nil) then
            ShowMessage('No individual DNA string can be generated.')
          else
            begin
              BCreateDNACreateClick(Sender);
              BCreateReadsCreateClick(nil);
            end;
          exit;
        end;
      lengthstring := ECreateReadsLength.Text;
      amountstring := ECreateReadsAmount.Text;
      misprobstring := ECreateReadsMisProb.Text;
      alphabetstring := ECreateDNAAlphabet.Text;

      varsl := TStringList.Create;
      varsl.Assign(MCreateDNAres.Lines);
      varsl.SaveToFile(MainPath + 'python\4_read_generator\individual.' + AnsiLowerCase(CBReferenceDNAFormat.Text));
      varsl.Free;

      runInPython('4_read_generator', MainPath + 'python\4_read_generator\individual.' + AnsiLowerCase(CBReferenceDNAFormat.Text) + #13#10 + lengthstring + #13#10 + amountstring + #13#10 + misprobstring + #13#10 + alphabetstring, FMain.CallbackCreateReadsCreated);
    end;
end;

procedure TFMain.CallbackCreateReadsCreated;
begin
  MCreateReadsres_reacttoonchange := false;

  MCreateReadsres.Lines.LoadFromFile(MainPath + 'python\4_read_generator\out.txt');

  if FileExists(MainPath + 'python\4_read_generator\outpos.txt') then
    MCreateReadspos.Lines.LoadFromFile(MainPath + 'python\4_read_generator\outpos.txt')
  else
    MCreateReadspos.Text := '';

  MCreateReadsres_reacttoonchange := true;
end;

procedure TFMain.CallbackPreProcessReference;
begin
  MPreProcessReference_reacttoonchange := false;

  if (CBuseBWT.Checked or CBuseBWTnew.Checked) then
    begin
      MPreProcessReferenceBWT.Lines.LoadFromFile(MainPath + 'python\2_reference_preprocessor\out.txt');
      MPreProcessReferenceSA.Lines.LoadFromFile(MainPath + 'python\2_reference_preprocessor\sa.txt');
    end
  else
    begin
      MPreProcessReference.Lines.LoadFromFile(MainPath + 'python\2_reference_preprocessor\out.txt');
      MPreProcessReference.Color := clWhite;
    end;

  MPreProcessReference_reacttoonchange := true;


  MPreProcessReferenceOutRef.Lines.LoadFromFile(MainPath + 'python\2_reference_preprocessor\outref.txt');

  showPreProcessReferenceOutRef := not (stringReplace(MPreProcessReferenceOutRef.Text, #13#10, '', [rfReplaceAll]) = stringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]));

  resetMemoHeights;
end;

procedure TFMain.TCheckExternalsTimer(Sender: TObject);
begin
  if FileExists(TCheckExternal_FilePath) then
    begin
      TCheckExternals.Enabled := false;
      TCheckExternal_Call;
    end;
end;

procedure TFMain.BOptionsClick(Sender: TObject);
begin
  FOptions.EPythonPath.Text := PythonPath;

  FOptions.CBpauseBatchfiles.Checked := pauseBatchfiles;

  FOptions.Show;
end;

procedure TFMain.SaveOptions;
var i: Integer;
begin
  if Length(FOptions.EPythonPath.Text) > 0 then
    PythonPath := FOptions.EPythonPath.Text;

  if AnsiLowerCase(copy(PythonPath, Length(PythonPath) - 9, 10)) = 'python.exe' then
    PythonPath := copy(PythonPath, 1, Length(PythonPath) - 10);

  i := Length(PythonPath);
  while (i > 0) and (PythonPath[i] = '\') do
    Dec(i);

  PythonPath := copy(PythonPath, 1, i) + '\';

  pauseBatchfiles := FOptions.CBpauseBatchfiles.Checked;

  SaveDAB;
end;

procedure TFMain.BReferenceDNACreateClick(Sender: TObject);
begin
  MReferenceDNA.Text := createRandomDNAString;
end;

procedure TFMain.CopyAll1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).SelectAll;
  (Screen.ActiveControl as TMemo).CopyToClipboard;
end;

procedure TFMain.PasteAll1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).SelectAll;
  (Screen.ActiveControl as TMemo).PasteFromClipboard;
end;

procedure TFMain.CopySelection1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).CopyToClipboard;
end;

procedure TFMain.PasteSelection1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).PasteFromClipboard;
end;

procedure TFMain.Undo1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).Undo;
end;

procedure TFMain.BReferenceDNAImportClick(Sender: TObject);
begin
  ODGeneral.Filter := 'All Files (*.*)|*.*';

  if ODGeneral.Execute then
    MReferenceDNA.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BCreateDNAImportClick(Sender: TObject);
begin
  ODGeneral.Filter := 'All Files (*.*)|*.*';

  if ODGeneral.Execute then
    MCreateDNAres.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BCreateReadsImportClick(Sender: TObject);
begin                   
  ODGeneral.Filter := 'All Files (*.*)|*.*';

  if ODGeneral.Execute then
    MCreateReadsres.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BReferenceDNAExportClick(Sender: TObject);
begin
  SDGeneral.Filter := 'All Files (*.*)|*.*';

  if SDGeneral.Execute then
    MReferenceDNA.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.BCreateDNAExportClick(Sender: TObject);
begin                              
  SDGeneral.Filter := 'All Files (*.*)|*.*';

  if SDGeneral.Execute then
    MCreateDNAres.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.BCreateReadsExportClick(Sender: TObject);
begin                   
  SDGeneral.Filter := 'All Files (*.*)|*.*';

  if SDGeneral.Execute then
    MCreateReadsres.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.MCreateDNAresChange(Sender: TObject);
begin
  RefreshCreateDNAStats;

  SBAllMemos.Max := Length(MCreateDNAres.Text);
end;

procedure TFMain.MReferenceDNAChange(Sender: TObject);
begin
  RefreshReferenceDNAStats;

  RefreshCreateDNAStats;

  SBAllMemos.Max := Length(MReferenceDNA.Text);

  showPreProcessReferenceOutRef := false;

  resetMemoHeights;
end;

procedure TFMain.RefreshReferenceDNAStats;
var reference, graphorstring, lengthkind: string;
    i: Integer;
    reflength: Extended;
begin
  graphorstring := 'string';
  lengthkind := '';

  if CBReferenceDNAFormat.Text = 'FASTA' then
    begin
      reference := '';
      i := 1;
      while i < MReferenceDNA.Lines.Count do
        begin
          reference := reference + MReferenceDNA.Lines[i];
          Inc(i);
        end;

      reflength := Length(reference);
    end
  else
    if CBReferenceDNAFormat.Text = 'FASTG' then
      begin
        LReferenceDNAStats.Caption := '(FASTG format cannot yet be interpreted to produce stats)';
        exit;
      end
    else
      if CBReferenceDNAFormat.Text = 'GFA' then
        begin
          LReferenceDNAStats.Caption := '(GFA format cannot yet be interpreted to produce stats)';
          exit;
        end
      else
        begin
          // STPU

          reference := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);

          if pos('(', reference) > 0 then
            begin
              graphorstring := 'graph';
              lengthkind := 'average ';
              reflength := countSTPUgraphAverageLength(reference);
            end
          else
            reflength := Length(reference);
        end;

  if Length(reference) = 0 then
    LReferenceDNAStats.Caption := 'Status: No reference DNA string present'
  else
    LReferenceDNAStats.Caption := 'Status: Reference DNA ' + graphorstring + ' has ' + lengthkind + 'length ' + FloattoStr(reflength);
end;

function TFMain.countSTPUgraphAverageLength(graph: string): Extended;
var countdown, curpos, startpos, graphlength: Integer;
begin
  result := 0;
  graphlength := Length(graph);

  curpos := pos('(', graph);
  startpos := curpos;

  if (curpos > 0) then
    begin
      result := curpos - 1;
      Inc(startpos);
    end;

  if (curpos < graphlength) then
    Inc(curpos);

  countdown := 0;

  while (curpos < graphlength) and ((not (graph[curpos] = ')')) or (countdown > 0)) do
    begin
      if (graph[curpos] = '(') then
        Inc(countdown);

      if (graph[curpos] = ')') then
        Dec(countdown);

      Inc(curpos);
    end;

  result := result + countAlternatives(copy(graph, startpos, curpos - startpos));

  if (curpos < graphlength) then
    result := result + countSTPUgraphAverageLength(copy(graph, curpos+1, graphlength));

end;

function TFMain.countAlternatives(graph: string): Extended;
var altcount, curpos, graphlength, lastfoundpos, countdown, goindeeper, gooutdeeper: Integer;
    totallength: Extended;
begin
  graphlength := Length(graph);

  altcount := 1;

  totallength := 0;
  curpos := 1;
  lastfoundpos := 0;
  countdown := 0;
  goindeeper := 0;

  while curpos < graphlength + 1 do
    begin
      if graph[curpos] = '(' then
        begin
          if countdown = 0 then
            goindeeper := curpos;

          inc(countdown);
        end;

      if graph[curpos] = ')' then
        begin
          dec(countdown);

          if countdown = 0 then
            begin
              gooutdeeper := curpos;

              totallength := totallength + goindeeper - (lastfoundpos + 1);

              totallength := totallength + countSTPUgraphAverageLength(copy(graph, goindeeper + 1, gooutdeeper - (goindeeper + 1)));

              lastfoundpos := gooutdeeper;
            end;
        end;

      if (graph[curpos] = '|') and (countdown = 0) then
        begin
          Inc(altcount);

          totallength := totallength + curpos - (lastfoundpos + 1);
          graph[curpos] := '_';
          lastfoundpos := curpos;
        end;

      inc(curpos);
    end;

  totallength := totallength + Length(graph) - lastfoundpos;

  result := totallength / altcount;
end;

procedure TFMain.RefreshCreateDNAStats;
var reference, individual: string;
    reflength, i: Integer;
begin
  if CBReferenceDNAFormat.Text = 'FASTA' then
    begin
      reference := '';
      i := 1;
      while i < MReferenceDNA.Lines.Count do
        begin
          reference := reference + MReferenceDNA.Lines[i];
          Inc(i);
        end;

      reflength := Length(reference);
    end
  else
    begin
      // STPU

      reference := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);

      reflength := Length(reference);

      if pos('(', reference) > 0 then
        begin
          LCreateDNAStats.Caption := 'Status: No status information available for graph references';
          exit;
        end;
    end;

  individual := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);

  if Length(individual) = 0 then
    LCreateDNAStats.Caption := 'Status: No individual DNA string present'
  else
    if Length(reference) = 0 then
      LCreateDNAStats.Caption := 'Status: Random individual DNA string, not based on any reference'
    else
      LCreateDNAStats.Caption := 'Status: Individual DNA string with ' + InttoStr(Length(individual) * 100 div reflength) + '% length of DNA reference string';
end;

procedure TFMain.BPreProcessReferenceClick(Sender: TObject);
var dnastring, lengthstring, useBWTstring: string;
begin
  if python_path_set then
    begin
      MPreProcessReference.Text := '';
      MPreProcessReferenceBWT.Text := '';
      MPreProcessReferenceSA.Text := '';

      dnastring := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);
      if Length(dnastring) = 0 then
        begin
          if (Sender = nil) then
            ShowMessage('No reference DNA string can be generated.')
          else
            begin
              BReferenceDNACreateClick(Sender);
              BPreProcessReferenceClick(nil);
            end;
          exit;
        end;
      lengthstring := EPreProcessReferenceHorizon.Text;
      useBWTstring := InttoStr(Integer(CBuseBWT.Checked) + 2 * Integer(CBuseHashes.Checked) + 4 * Integer(CBuseBWTnew.Checked));

      MReferenceDNA.Lines.SaveToFile(MainPath + 'python\2_reference_preprocessor\reference.' + AnsiLowerCase(CBReferenceDNAFormat.Text));

      runInPython('2_reference_preprocessor', MainPath + 'python\2_reference_preprocessor\reference.' + AnsiLowerCase(CBReferenceDNAFormat.Text) + #13#10 + lengthstring + #13#10 + useBWTstring, FMain.CallbackPreProcessReference);
    end;
end;

procedure TFMain.runInPython(folder, input: string; callOnFinish: TCallbackMethod);
var output: TStringList;
    FilePath, InFilePath, OutFilePath: string;
begin
  output := TStringList.Create;

  output.Text := input;

  FilePath := MainPath + 'python\' + folder + '\';
  InFilePath := FilePath + 'in.txt';
  OutFilePath := FilePath + 'out.txt';
  DeleteFile(OutFilePath);

  output.SaveToFile(InFilePath);

  output.Text := InFilePath[1] + ':' + #13#10 +
  'cd ' + copyend(InFilePath, '\', 1) + #13#10 +
  PythonPath + 'python.exe ' + FilePath + 'e.py';

  if pauseBatchfiles then
    output.Add('pause');

  output.SaveToFile(Path + 'batch.bat');
  output.Free;

  ShellExecute(Application.handle, PChar('open'), PChar('batch.bat'), PChar(''), PChar(Path), SW_SHOW);

  TCheckExternal_FilePath := OutFilePath;
  TCheckExternal_Call := callOnFinish;
  TCheckExternals.Enabled := true;
end;

procedure TFMain.BAlignReadsClick(Sender: TObject);
var refpath, useBWTstring, usepigeonholestring: string;
begin
  if python_path_set then
    begin
      MAlignReads.Text := '';

      if (MReferenceDNA.Text = '') or (MReferenceDNA.Text = #13#10) then
        begin
          if (Sender = nil) then
            ShowMessage('No reference DNA string can be generated.')
          else
            begin
              BReferenceDNACreateClick(Sender);
              BPreProcessReferenceClick(Sender);
              BAlignReadsClick(nil);
            end;
          exit;
        end;

      // optimization opportunity: we don't even really need to save here, as we already saved the reference to this location in step 2
      // of course, this assumes that we go through the steps one after the other, which is not necessarily always the case
      // also, this assumes that no one played around with the reference in the meantime - and the whole point of the MCC is to allow
      // its users to play around with it in real time, in between the execution of steps =)

      if CBuseBWT.Checked or CBuseBWTnew.Checked then
        begin
          // BWT [Siren2014] always uses the regular reference
          refpath := MainPath + 'python\2_reference_preprocessor\reference.' + AnsiLowerCase(CBReferenceDNAFormat.Text);
          MReferenceDNA.Lines.SaveToFile(refpath);
        end
      else
        if CBuseHashes.Checked then
          begin
            // hashes [Schneeberger2009] may use a pre-processed reference in step 4
            if showPreProcessReferenceOutRef then
              begin
                refpath := MainPath + 'python\2_reference_preprocessor\outref.' + AnsiLowerCase(CBReferenceDNAFormat.Text);
                MPreProcessReferenceOutRef.Lines.SaveToFile(refpath);
              end
            else
              begin
                refpath := MainPath + 'python\2_reference_preprocessor\reference.' + AnsiLowerCase(CBReferenceDNAFormat.Text);
                MReferenceDNA.Lines.SaveToFile(refpath);
              end;
          end
        else
          begin           
            // hashes [now] may use a pre-processed reference in step 4
            if showPreProcessReferenceOutRef then
              begin
                refpath := MainPath + 'python\2_reference_preprocessor\outref.' + AnsiLowerCase(CBReferenceDNAFormat.Text);
                MPreProcessReferenceOutRef.Lines.SaveToFile(refpath);
              end
            else
              begin
                refpath := MainPath + 'python\2_reference_preprocessor\reference.' + AnsiLowerCase(CBReferenceDNAFormat.Text);
                MReferenceDNA.Lines.SaveToFile(refpath);
              end;
          end;

      useBWTstring := InttoStr(Integer(CBuseBWT.Checked) + 2 * Integer(CBuseHashes.Checked) + 4 * Integer(CBuseBWTnew.Checked));
      usepigeonholestring := InttoStr(Integer(CBusePigeonhole.Checked));

      runInPython('5_read_aligner', refpath + #13#10 + MainPath + 'python\2_reference_preprocessor\out.txt' + #13#10 + MainPath + 'python\2_reference_preprocessor\sa.txt' + #13#10 + MainPath + 'python\4_read_generator\out.txt' + #13#10 + EAlignReadsD.Text + #13#10 + EAlignReadsK.Text + #13#10 + EAlignReadsLength.Text + #13#10 + useBWTstring + #13#10 + usepigeonholestring, FMain.CallbackAlignReads);
    end;
end;

procedure TFMain.CallbackAlignReads;
begin
  MAlignReads_reacttoonchange := false;

  MAlignReads.Lines.LoadFromFile(MainPath + 'python\5_read_aligner\out.txt');

  MAlignReads_reacttoonchange := true;
end;

function TFMain.python_path_set: Boolean;
begin
  result := Length(PythonPath) > 0;

  if not result then
    ShowMessage('Please first go to the options page and specify a valid path for python.');
end;

procedure TFMain.BAssembleDNAClick(Sender: TObject);
var reference: string;
    i: Integer;
    refMemo: TMemo;
begin
  if python_path_set then
    begin
      if (PPreProcessReferenceWOBWT.Visible and showPreProcessReferenceOutRef) then
        refMemo := MPreProcessReferenceOutRef
      else
        refMemo := MReferenceDNA;

      MAssembleDNA.Text := '';

      if CBReferenceDNAFormat.Text = 'FASTA' then
        begin
          reference := '';
          i := 1;
          while i < refMemo.Lines.Count do
            begin
              reference := reference + refMemo.Lines[i];
              Inc(i);
            end;
        end
      else
        begin
          // STPU

          reference := StringReplace(refMemo.Text, #13#10, '', [rfReplaceAll]);
        end;

      if Length(reference) = 0 then
        begin
          BAlignReadsClick(Sender);
          ShowMessage('Please try again after the alignment is done.');
          exit;
        end;

      runInPython('6_string_assembler', InttoStr(Length(reference)) + #13#10 + MainPath + 'python\4_read_generator\out.txt' + #13#10 + MainPath + 'python\5_read_aligner\out.txt' + #13#10 + AnsiLowerCase(CBReferenceDNAFormat.Text), FMain.CallbackAssembleDNA);
    end;
end;

procedure TFMain.CallbackAssembleDNA;
var varsl: TStringList;
begin
  MAssembleDNA_reacttoonchange := false;

  varsl := TStringList.Create;
  varsl.LoadFromFile(MainPath + 'python\6_string_assembler\out.txt');

  AssembleDNA_quality := varsl[0];
  varsl.Delete(0);

  MAssembleDNA.Text := varsl.Text;
  varsl.Free;

  MAssembleDNA_reacttoonchange := true;
end;

procedure TFMain.SelectAll1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).SelectAll;
end;

procedure TFMain.CutAll1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).SelectAll;
  (Screen.ActiveControl as TMemo).CutToClipboard;
end;

procedure TFMain.CutSelection1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).CutToClipboard;
end;

procedure TFMain.BAlignReadsExportClick(Sender: TObject);
begin                     
  SDGeneral.Filter := 'All Files (*.*)|*.*';

  if SDGeneral.Execute then
    MAlignReads.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.BAssembleDNAExportClick(Sender: TObject);
begin                     
  SDGeneral.Filter := 'All Files (*.*)|*.*';

  if SDGeneral.Execute then
    MAssembleDNA.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.BPreProcessReferenceImportClick(Sender: TObject);
begin               
  ODGeneral.Filter := 'All Files (*.*)|*.*';

  if ODGeneral.Execute then
    MPreProcessReference.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BPreProcessReferenceExportClick(Sender: TObject);
begin                  
  SDGeneral.Filter := 'All Files (*.*)|*.*';

  if SDGeneral.Execute then
    MPreProcessReference.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.MAssembleDNAChange(Sender: TObject);
begin
  if (MAssembleDNA_reacttoonchange) then
    AssembleDNA_quality := 'nil';

  RefreshAssembleDNAStats;

  SBAllMemos.Max := Length(MAssembleDNA.Text);
end;

procedure TFMain.RefreshAssembleDNAStats;
begin
  if not (lastMAssembleDNAtext = MAssembleDNA.Text) then
    begin
      lastMAssembleDNAtext := MAssembleDNA.Text;
      RefreshAssemblyStats(LAssembleDNAStatus, MAssembleDNA);
      if not (AssembleDNA_quality = 'nil') then
        LAssembleDNAStatus.Caption := LAssembleDNAStatus.Caption + ', assembly quality ' + AssembleDNA_quality;
    end;
end;

procedure TFMain.RefreshAssemblyStats(resLabel: TLabel; assemblyMemo: TRichEdit);
var res, original, assembledDNA: string;
    i, correct, noteventried, minlen, origSelStart, origSelLength: Integer;
    colorMeInterested: Boolean;
begin
  res := 'Status: ';

  colorMeInterested := false;

  origSelStart := assemblyMemo.SelStart;
  origSelLength := assemblyMemo.SelLength;

  assemblyMemo.SelectAll;
  assemblyMemo.SelAttributes.Color := assemblyMemo.Font.Color;
  assemblyMemo.SelAttributes.Style := [];

  if CBReferenceDNAFormat.Text = 'FASTA' then
    begin
      original := '';
      i := 1;
      while i < MCreateDNAres.Lines.Count do
        begin
          original := original + MCreateDNAres.Lines[i] + #13#10;
          Inc(i);
        end;

      assembledDNA := '';
      i := 1;
      while i < assemblyMemo.Lines.Count do
        begin
          assembledDNA := assembledDNA + assemblyMemo.Lines[i] + #13#10;
          Inc(i);
        end;

      // get rid of trailing #13#10
      original := copy(original, 1, Length(original) - 2);
      assembledDNA := copy(assembledDNA, 1, Length(assembledDNA) - 2);
    end
  else
    begin
      // STPU

      original := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);
      assembledDNA := StringReplace(assemblyMemo.Text, #13#10, '', [rfReplaceAll]);

      colorMeInterested := true;
    end;

  if Length(assembledDNA) = 0 then
    res := res + 'Assembly not initialized'
  else
    if Length(original) = 0 then
      res := res + 'No individual DNA string present'
    else
      begin
        correct := 0;
        noteventried := 0;

        if (colorMeInterested) then
          begin
            for i := 1 to min(Length(assembledDNA), Length(original)) do
              begin
                assemblyMemo.SelStart := i - 1;
                assemblyMemo.SelLength := 1;

                if assembledDNA[i] = original[i] then
                  begin
                    Inc(correct);
                    assemblyMemo.SelAttributes.Color := clGreen;
                  end
                else
                  if assembledDNA[i] = '_' then
                    begin
                      Inc(noteventried);
                      assemblyMemo.SelAttributes.Color := clBlue;
                    end
                  else
                    assemblyMemo.SelAttributes.Color := clRed;
              end;
          end
        else
          begin
            for i := 1 to min(Length(assembledDNA), Length(original)) do
              if assembledDNA[i] = original[i] then
                Inc(correct)
              else
                if assembledDNA[i] = '_' then
                  Inc(noteventried);
          end;
        minlen := min(Length(assembledDNA), Length(original));
        res := res + 'Assembly to ' + InttoStr((correct * 100) div minlen) + '% correct, to ' + InttoStr((noteventried * 100) div minlen) + '% stated as unknown and to ' + InttoStr(100 - (((correct + noteventried) * 100) div minlen)) + '% stated wrongly';
      end;

  resLabel.Caption := res;

  assemblyMemo.SelStart := origSelStart;
  assemblyMemo.SelLength := origSelLength;
end;

procedure TFMain.EPreProcessReferenceHorizonChange(Sender: TObject);
begin
  if not (EPreProcessReferenceHorizon.Text = EAlignReadsLength.Text) then
    EAlignReadsLength.Text := EPreProcessReferenceHorizon.Text;

  if Length(MPreProcessReference.Text) > 0 then
    MPreProcessReference.Color := clRed;
end;

procedure TFMain.MCreateReadsresChange(Sender: TObject);
begin
  RefreshCreateReadsStats;

  if MCreateReadsres_reacttoonchange then
    begin
      MCreateReadsres.Lines.SaveToFile(MainPath + 'python\4_read_generator\out.txt');
      MCreateReadspos.Text := '';
    end;
end;

procedure TFMain.MCreateReadsposChange(Sender: TObject);
begin
  RefreshCreateReadsStats;
  RefreshAlignReadsStats;
end;

procedure TFMain.RefreshCreateReadsStats;
var res, original: string;
    overalllength, i, j, b, e: Integer;
    Readsres, Readspos: TStringList;
begin
  res := 'Status: ';

  Readsres := TStringList.Create;
  Readsres.Assign(MCreateReadsres.Lines);
  Readspos := TStringList.Create;
  Readspos.Assign(MCreateReadspos.Lines);

  if CBReferenceDNAFormat.Text = 'FASTA' then
    begin
      original := '';
      i := 1;
      while i < MCreateDNAres.Lines.Count do
        begin
          original := original + MCreateDNAres.Lines[i];
          Inc(i);
        end;
    end
  else
    begin
      // STPU

      original := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);
    end;

  if Readsres.Count = 0 then
    res := res + 'No reads created'
  else
    if Readspos.Count = 0 then
      res := res + 'No location information available'
    else
      if Length(original) = 0 then
        res := res + 'No individual DNA string created'
      else
        begin
          overalllength := Length(original);
          for i := 0 to Readsres.Count - 1 do
            if i < Readspos.Count then
              begin
                b := StrToInt(Readspos[i]) + 1;
                e := StrToInt(Readspos[i]) + Length(Readsres[i]);
                for j := b to e do
                  if j <= overalllength then
                    original[j] := '_';
              end
            else
              break;

          j := 0;
          for i := 1 to overalllength do
            if original[i] = '_' then
              Inc(j);

          res := InttoStr((j * 100) div overalllength) + '% coverage of individual DNA string by reads';
        end;

  LCreateReadsStatus.Caption := res;

  Readsres.Free;
  Readspos.Free;
end;

procedure TFMain.RefreshAlignReadsStats;
var res, arline: string;
    Readspos, Alignedreads: TStringList;
    fulllength, correctlyaligned, noteventried, i, dist_avg: Integer;
begin
  res := 'Status: ';

  Readspos := TStringList.Create;
  Readspos.Assign(MCreateReadspos.Lines);
  Alignedreads := TStringList.Create;
  Alignedreads.Assign(MAlignReads.Lines);

  if Alignedreads.Count = 0 then
    res := res + 'Alignment not initialized'
  else
    if Readspos.Count = 0 then
      res := res + 'No location information available'
    else
      begin
        fulllength := min(Readspos.Count, Alignedreads.Count) - 1;
        correctlyaligned := 0;
        noteventried := 0;
        dist_avg := 0;

        for i := 0 to fulllength do
          begin
            arline := Alignedreads[i];

            // for now, simply take the first numerical value
            // TODO :: however, later on we should examine more carefully what is going on!
            // that is, we should also analyse the infostr AND we should make up for indels in the reference!
            // (the easiest way for that would be to already keep track of indels that are made when the individual DNA is created...)
            if pos('[', arline) > 0 then
              arline := copy(arline, 2, pos(', ', arline) - 2);

            if Readspos[i] = arline then
              Inc(correctlyaligned)
            else
              if arline = '' then
                Inc(noteventried)
              else
                dist_avg := dist_avg + Abs(StrtoInt(Readspos[i]) - StrtoInt(arline))
          end;

        if fulllength > -1 then
          dist_avg := 100 * dist_avg div (fulllength + 1);

        res := res + 'Alignment to ' + InttoStr((correctlyaligned * 100) div (fulllength + 1)) + '% correct, to ' + InttoStr((noteventried * 100) div (fulllength + 1)) + '% stated as unknown and to ' + InttoStr(100 - (((correctlyaligned + noteventried) * 100) div (fulllength + 1))) + '% stated wrongly. Average distance between aligned and real locations is ' + FloatyInttoStr(dist_avg, '.') + ' basepairs.';
      end;

  Readspos.Free;
  Alignedreads.Free;

  LAlignReadsStatus.Caption := res;
end;

procedure TFMain.MAlignReadsChange(Sender: TObject);
begin
  RefreshAlignReadsStats;

  if MAlignReads_reacttoonchange then
    MAlignReads.Lines.SaveToFile(MainPath + 'python\5_read_aligner\out.txt');
end;

procedure TFMain.EAlignReadsLengthChange(Sender: TObject);
begin
  if not (EPreProcessReferenceHorizon.Text = EAlignReadsLength.Text) then
    EPreProcessReferenceHorizon.Text := EAlignReadsLength.Text;
end;

procedure TFMain.SBAllMemosScroll(Sender: TObject; ScrollCode: TScrollCode;
var ScrollPos: Integer);
begin
  MReferenceDNA.SelStart := 0;
  MReferenceDNA.SelLength := 0;
  MReferenceDNA.Perform(EM_SCROLLCARET, 0, 0);

  MPreProcessReferenceOutRef.SelStart := 0;
  MPreProcessReferenceOutRef.SelLength := 0;
  MPreProcessReferenceOutRef.Perform(EM_SCROLLCARET, 0, 0);

  MCreateDNAres.SelStart := 0;
  MCreateDNAres.SelLength := 0;
  MCreateDNAres.Perform(EM_SCROLLCARET, 0, 0);

  MAssembleDNA.SelStart := 0;
  MAssembleDNA.SelLength := 0;
  MAssembleDNA.Perform(EM_SCROLLCARET, 0, 0);

  MFillGaps.SelStart := 0;
  MFillGaps.SelLength := 0;
  MFillGaps.Perform(EM_SCROLLCARET, 0, 0);


  MReferenceDNA.SelStart := 10000;
  MReferenceDNA.SelLength := 0;
  MReferenceDNA.Perform(EM_SCROLLCARET, 0, 0);

  MPreProcessReferenceOutRef.SelStart := 10000;
  MPreProcessReferenceOutRef.SelLength := 0;
  MPreProcessReferenceOutRef.Perform(EM_SCROLLCARET, 0, 0);

  MCreateDNAres.SelStart := 10000;
  MCreateDNAres.SelLength := 0;
  MCreateDNAres.Perform(EM_SCROLLCARET, 0, 0);

  MAssembleDNA.SelStart := 10000;
  MAssembleDNA.SelLength := 0;
  MAssembleDNA.Perform(EM_SCROLLCARET, 0, 0);

  MFillGaps.SelStart := 10000;
  MFillGaps.SelLength := 0;
  MFillGaps.Perform(EM_SCROLLCARET, 0, 0);


  MReferenceDNA.SelStart := SBAllMemos.Position;
  MReferenceDNA.SelLength := 0;
  MReferenceDNA.Perform(EM_SCROLLCARET, 0, 0);

  MPreProcessReferenceOutRef.SelStart := SBAllMemos.Position;
  MPreProcessReferenceOutRef.SelLength := 0;
  MPreProcessReferenceOutRef.Perform(EM_SCROLLCARET, 0, 0);

  MCreateDNAres.SelStart := SBAllMemos.Position;
  MCreateDNAres.SelLength := 0;
  MCreateDNAres.Perform(EM_SCROLLCARET, 0, 0);

  MAssembleDNA.SelStart := SBAllMemos.Position;
  MAssembleDNA.SelLength := 0;
  MAssembleDNA.Perform(EM_SCROLLCARET, 0, 0);

  MFillGaps.SelStart := SBAllMemos.Position;
  MFillGaps.SelLength := 0;
  MFillGaps.Perform(EM_SCROLLCARET, 0, 0);

  {
  This solution would be cleaner, but RichEdits don't support it:

  MReferenceDNA.Lines.BeginUpdate;
  MReferenceDNA.Perform(EM_LineScroll,-SBAllMemos.Max,0);
  MReferenceDNA.Perform(EM_LineScroll,SBAllMemos.Position,0);
  MReferenceDNA.Lines.EndUpdate;

  MCreateDNAres.Lines.BeginUpdate;
  MCreateDNAres.Perform(EM_LineScroll,-SBAllMemos.Max,0);
  MCreateDNAres.Perform(EM_LineScroll,SBAllMemos.Position,0);
  MCreateDNAres.Lines.EndUpdate;

  MAssembleDNA.Lines.BeginUpdate;
  MAssembleDNA.Perform(EM_LineScroll,-SBAllMemos.Max,0);
  MAssembleDNA.Perform(EM_LineScroll,SBAllMemos.Position,0);
  MAssembleDNA.Lines.EndUpdate;

  MFillGaps.Lines.BeginUpdate;
  MFillGaps.Perform(EM_LineScroll,-SBAllMemos.Max,0);
  MFillGaps.Perform(EM_LineScroll,SBAllMemos.Position,0);
  MFillGaps.Lines.EndUpdate;
  }
end;

procedure TFMain.BFillGapsExportClick(Sender: TObject);
begin                       
  SDGeneral.Filter := 'All Files (*.*)|*.*';

  if SDGeneral.Execute then
    MFillGaps.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.MFillGapsChange(Sender: TObject);
begin
  RefreshFillGapsStats;

  SBAllMemos.Max := Length(MFillGaps.Text);
end;

procedure TFMain.RefreshFillGapsStats;
begin                          
  if not (lastMFillGapstext = MFillGaps.Text) then
    begin
      lastMFillGapstext := MFillGaps.Text;
      RefreshAssemblyStats(LFillGapsStatus, MFillGaps);
    end;
end;

procedure TFMain.BFillGapsClick(Sender: TObject);
var assembledDNA, reference: string;
    i, minlen: Integer;
    refMemo: TMemo;
begin
  if (PPreProcessReferenceWOBWT.Visible and showPreProcessReferenceOutRef) then
    refMemo := MPreProcessReferenceOutRef
  else
    refMemo := MReferenceDNA;

  if CBReferenceDNAFormat.Text = 'FASTA' then
    begin
      reference := '';
      i := 1;
      while i < refMemo.Lines.Count do
        begin
          reference := reference + refMemo.Lines[i] + #13#10;
          Inc(i);
        end;

      assembledDNA := '';
      i := 1;
      while i < MAssembleDNA.Lines.Count do
        begin
          assembledDNA := assembledDNA + MAssembleDNA.Lines[i] + #13#10;
          Inc(i);
        end;
    end
  else
    begin
      // STPU

      reference := StringReplace(refMemo.Text, #13#10, '', [rfReplaceAll]);
      assembledDNA := StringReplace(MAssembleDNA.Text, #13#10, '', [rfReplaceAll]);
    end;

  minlen := min(Length(assembledDNA), Length(reference));

  for i := 1 to minlen do
    if assembledDNA[i] = '_' then
      assembledDNA[i] := reference[i];

  if CBReferenceDNAFormat.Text = 'FASTA' then
    begin
      if (MAssembleDNA.Lines.Count > 0) then
        assembledDNA := MAssembleDNA.Lines.Strings[0] + #13#10 + copy(assembledDNA, 1, Length(assembledDNA) - 2);
    end;

  MFillGaps.Text := assembledDNA;
end;

procedure TFMain.MPreProcessReferenceChange(Sender: TObject);
begin
  RefreshPreProcessReferenceStats;

  if (MPreProcessReference_reacttoonchange) then
    MPreProcessReference.Lines.SaveToFile(MainPath + 'python\2_reference_preprocessor\out.txt');
end;

procedure TFMain.RefreshPreProcessReferenceStats;
var prepro, reference: string;
    preprocount, i: Integer;
begin
  prepro := StringReplace(MPreProcessReference.Text, #13#10, '', [rfReplaceAll]);
  preprocount := MPreProcessReference.Lines.Count;

  if CBReferenceDNAFormat.Text = 'FASTA' then
    begin
      reference := '';
      i := 1;
      while i < MReferenceDNA.Lines.Count do
        begin
          reference := reference + MReferenceDNA.Lines[i];
          Inc(i);
        end;
    end
  else
    begin
      // STPU

      reference := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);
    end;

  if prepro = '' then
    LPreProcessReferenceStats.Caption := 'Status: No pre-processing has occurred'
  else
    LPreProcessReferenceStats.Caption := 'Status: Pre-processing obtained ' + InttoStr(preprocount) + ' entries (' + InttoStr((100 * preprocount) div Length(reference)) + '% coverage of reference)';
end;

procedure TFMain.RefreshStatistics1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).OnChange(Sender);
end;

// clears all the data
procedure TFMain.clearEverything;
begin
  MReferenceDNA.Text := '';
  MPreProcessReference.Text := '';
  MPreProcessReferenceOutRef.Text := '';
  MCreateDNAres.Text := '';
  MCreateReadsres.Text := '';
  MCreateReadspos.Text := '';
  MAlignReads.Text := '';
  MAssembleDNA.Text := '';
  MFillGaps.Text := '';
  MAssembleDNAShowAlign.Text := '';

  showPreProcessReferenceOutRef := false;
end;

procedure TFMain.CBReferenceDNAFormatChange(Sender: TObject);
begin
  clearEverything;

  resetInterface;
end;

procedure TFMain.resetInterface;
begin
  GBPreProcessReference.Visible := true;
  GBCreateDNA.Visible := true;
  GBCreateReads.Visible := true;
  GBAlignReads.Visible := true;
  GBAssembleDNA.Visible := true;
  GBFillGaps.Visible := true;

  resetMemoHeights;
end;

procedure TFMain.resetMemoHeights;
var newheight: Integer;
begin
  PPreProcessReferenceWBWT.Visible := CBuseBWT.Checked or CBuseBWTnew.Checked;
  PPreProcessReferenceWOBWT.Visible := CBuseHashesNew.Checked or CBuseHashes.Checked;

  if (CBReferenceDNAFormat.Text = 'STPU') then
    newheight := 41
  else
    newheight := 200;

  MReferenceDNA.Height := newheight;
  if CBReferenceDNAGraph.Checked then
    begin
      MReferenceDNA.Top := 128;
      CBReferenceDNAFormat.Top := 100;
      LReferenceDNAFormat.Top := 103;
      GBReferenceDNA.Height := 152 + newheight;
      LReferenceDNAStats.Top := 131 + newheight;
      RBReferenceDNAGraphSnips.Visible := true;
      RBReferenceDNAGraphMultiSnips.Visible := true;
      RBReferenceDNAGraphFullMultiSnips.Visible := true;
      RBReferenceDNAGraphFull.Visible := true;
    end
  else
    begin
      MReferenceDNA.Top := 104;
      CBReferenceDNAFormat.Top := 76;
      LReferenceDNAFormat.Top := 79;
      GBReferenceDNA.Height := 128 + newheight;
      LReferenceDNAStats.Top := 107 + newheight;
      RBReferenceDNAGraphSnips.Visible := false;
      RBReferenceDNAGraphMultiSnips.Visible := false;
      RBReferenceDNAGraphFullMultiSnips.Visible := false;
      RBReferenceDNAGraphFull.Visible := false;
    end;

  GBPreProcessReference.Top := GBReferenceDNA.Top + GBReferenceDNA.Height + 8;

  MPreProcessReferenceOutRef.Height := newheight;

  if (PPreProcessReferenceWOBWT.Visible) then
    begin
      if showPreProcessReferenceOutRef then
        GBPreProcessReference.Height := 224 + newheight
      else
        GBPreProcessReference.Height := 193;

      PPreProcessReferenceWOBWT.Height := GBPreProcessReference.Height - 42;
    end
  else
    begin
      GBPreProcessReference.Height := 241;
      PPreProcessReferenceWBWT.Height := GBPreProcessReference.Height - 42;
    end;

  if GBPreProcessReference.Visible then
    GBCreateDNA.Top := GBPreProcessReference.Top + GBPreProcessReference.Height + 8
  else
    GBCreateDNA.Top := GBPreProcessReference.Top;

  MCreateDNAres.Height := newheight;
  GBCreateDNA.Height := 144 + newheight;
  LCreateDNAStats.Top := 123 + newheight;

  if GBCreateDNA.Visible then
    GBCreateReads.Top := GBCreateDNA.Top + GBCreateDNA.Height + 8
  else
    GBCreateReads.Top := GBCreateDNA.Top;

  if GBCreateReads.Visible then
    GBAlignReads.Top := GBCreateReads.Top + GBCreateReads.Height + 8
  else
    GBAlignReads.Top := GBCreateReads.Top;

  if GBAlignReads.Visible then
    GBAssembleDNA.Top := GBAlignReads.Top + GBAlignReads.Height + 8
  else
    GBAssembleDNA.Top := GBAlignReads.Top;

  MAssembleDNA.Height := newheight;
  GBAssembleDNA.Height := 208 + newheight;
  LAssembleDNAStatus.Top := 51 + newheight;
  BAssembleDNAShowAlign.Top := 71 + newheight;
  MAssembleDNAShowAlign.Top := 103 + newheight;

  if GBAssembleDNA.Visible then
    GBFillGaps.Top := GBAssembleDNA.Top + GBAssembleDNA.Height + 8
  else
    GBFillGaps.Top := GBAssembleDNA.Top;

  MFillGaps.Height := newheight;
  GBFillGaps.Height := 88 + newheight;
  LFillGapsStatus.Top := 67 + newheight;

  if GBFillGaps.Visible then
    GBCallVariants.Top := GBFillGaps.Top + GBFillGaps.Height + 8
  else
    GBCallVariants.Top := GBFillGaps.Top;

  if GBCallVariants.Visible then
    GBAddNotes.Top := GBCallVariants.Top + GBCallVariants.Height + 8
  else
    GBAddNotes.Top := GBCallVariants.Top;

  if GBAddNotes.Visible then
    PMain.Height := GBAddNotes.Top + GBAddNotes.Height + 8
  else
    PMain.Height := GBAddNotes.Top;

  // this is necessary as the main scroll box might add or remove its vertical
  // scroll bar =)
  FormResize(nil);
end;

procedure TFMain.CBReferenceDNAGraphClick(Sender: TObject);
begin
  if CBReferenceDNAGraph.Checked and (CBReferenceDNAFormat.Text = 'FASTA') then
    begin
      CBReferenceDNAGraph.Checked := false;
      ShowMessage('Sorry, but the FASTA format does not support reference graphs.');
    end;

  resetMemoHeights;

  resetInterface;
end;

procedure TFMain.ECreateReadsLengthChange(Sender: TObject);
begin
  if (Length(ECreateReadsLength.Text) > 0) and (Length(EAlignReadsD.Text) > 0) then
    EAlignReadsLength.Text := InttoStr(ceil(StrtoInt(ECreateReadsLength.Text) / (StrtoInt(EAlignReadsD.Text) + 1)));
end;

procedure TFMain.RBReferenceDNAGraphSnipsClick(Sender: TObject);
begin
  resetInterface;
end;

procedure TFMain.RBReferenceDNAGraphFullClick(Sender: TObject);
begin     
  resetInterface;
end;

procedure TFMain.CBuseBWTClick(Sender: TObject);
begin
  MPreProcessReference.Text := '';

  showPreProcessReferenceOutRef := false;

  resetMemoHeights;
end;

procedure TFMain.BPreProcessReferenceBWTClick(Sender: TObject);
var dnastring, lengthstring, useBWTstring: string;
begin
  if python_path_set then
    begin
      MPreProcessReference.Text := '';

      dnastring := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);
      if Length(dnastring) = 0 then
        begin
          if (Sender = nil) then
            ShowMessage('No reference DNA string can be generated.')
          else
            begin
              BReferenceDNACreateClick(Sender);
              BPreProcessReferenceClick(nil);
            end;
          exit;
        end;
      lengthstring := EPreProcessReferenceHorizon.Text;
      useBWTstring := InttoStr(Integer(CBuseBWT.Checked) + 2 * Integer(CBuseHashes.Checked) + 4 * Integer(CBuseBWTnew.Checked));

      MReferenceDNA.Lines.SaveToFile(MainPath + 'python\2_reference_preprocessor\reference.' + AnsiLowerCase(CBReferenceDNAFormat.Text));

      runInPython('2_reference_preprocessor', MainPath + 'python\2_reference_preprocessor\reference.' + AnsiLowerCase(CBReferenceDNAFormat.Text) + #13#10 + lengthstring + #13#10 + useBWTstring, FMain.CallbackPreProcessReference);
    end;
end;

procedure TFMain.BExportAllClick(Sender: TObject);
var varsl: TStringList;
    filename, fileext: string;
    filenamelength: Integer;
begin
  SDGeneral.Filter := 'Main Central Control Files (*.mcc)|*.mcc|All Files (*.*)|*.*';

  if SDGeneral.Execute then
    begin
      filename := SDGeneral.FileName;
      filenamelength := Length(filename);
      if filenamelength > 3 then
        begin
          fileext := 'mcc';

          if filename[filenamelength] = '.' then
            filename := copy(filename, 1, filenamelength-1)
          else
            if filename[filenamelength-1] = '.' then
              begin
                fileext := filename[filenamelength];
                filename := copy(filename, 1, filenamelength-2);
              end
            else
              if filename[filenamelength-2] = '.' then
                begin
                  fileext := copy(filename, filenamelength-1, 2);
                  filename := copy(filename, 1, filenamelength-3);
                end
              else
                if filename[filenamelength-3] = '.' then
                  begin
                    fileext := copy(filename, filenamelength-2, 3);
                    filename := copy(filename, 1, filenamelength-4);
                  end;

          fileext := '.' + fileext;

          varsl := TStringList.Create;
          varsl.Text := version + #13#10 +
          InttoStr(Integer(CBReferenceDNAGraph.Checked)) + #13#10 +
          ECreateDNALength.Text + #13#10 +
          ECreateDNAAlphabet.Text + #13#10 +
          EPreProcessReferenceHorizon.Text + #13#10 +
          ECreateReadsLength.Text + #13#10 +
          ECreateReadsAmount.Text + #13#10 +
          ECreateReadsMisProb.Text + #13#10 +
          EAlignReadsLength.Text + #13#10 +
          EAlignReadsD.Text + #13#10 +
          EAlignReadsK.Text + #13#10 +
          ECreateDNAEditsLike.Text + #13#10 +
          ECreateDNAEditsContLike.Text + #13#10 +
          ECreateDNAInsertsLike.Text + #13#10 +
          ECreateDNAInsertsContLike.Text + #13#10 +
          ECreateDNADeletionsLike.Text + #13#10 +
          ECreateDNADeletionsContLike.Text + #13#10 +
          CBReferenceDNAFormat.Text + #13#10 +
          InttoStr(Integer(RBReferenceDNAGraphSnips.Checked)) + #13#10 +
          InttoStr(Integer(RBReferenceDNAGraphMultiSnips.Checked)) + #13#10 +
          InttoStr(Integer(RBReferenceDNAGraphFullMultiSnips.Checked)) + #13#10 +
          InttoStr(Integer(RBReferenceDNAGraphFull.Checked)) + #13#10 +
          InttoStr(Integer(CBuseHashesNew.Checked)) + #13#10 +
          InttoStr(Integer(CBuseHashes.Checked)) + #13#10 +
          InttoStr(Integer(CBuseBWT.Checked)) + #13#10 +
          InttoStr(Integer(CBusePigeonhole.Checked)) + #13#10 +
          InttoStr(Integer(showPreProcessReferenceOutRef)) + #13#10 +
          InttoStr(Integer(CBuseBWTnew.Checked));

          varsl.SaveToFile(filename + fileext);
          varsl.Free;

          fileext := '.mcm';

          SaveFromMemo(MReferenceDNA, filename + '_MReferenceDNA' + fileext);
          SaveFromMemo(MPreProcessReferenceBWT, filename + '_MPreProcessReferenceBWT' + fileext);
          SaveFromMemo(MPreProcessReferenceSA, filename + '_MPreProcessReferenceSA' + fileext);
          SaveFromMemo(MPreProcessReference, filename + '_MPreProcessReference' + fileext);
          SaveFromMemo(MPreProcessReferenceOutRef, filename + '_MPreProcessReferenceOutRef' + fileext);
          SaveFromMemo(MCreateDNAres, filename + '_MCreateDNAres' + fileext);
          SaveFromMemo(MCreateReadsres, filename + '_MCreateReadsres' + fileext);
          SaveFromMemo(MCreateReadspos, filename + '_MCreateReadspos' + fileext);
          SaveFromMemo(MAlignReads, filename + '_MAlignReads' + fileext);
          SaveFromMemo(MAssembleDNA, filename + '_MAssembleDNA' + fileext);
          SaveFromMemo(MAssembleDNAShowAlign, filename + '_MAssembleDNAShowAlign' + fileext);
          SaveFromMemo(MFillGaps, filename + '_MFillGaps' + fileext);
          SaveFromMemo(MCallVariantsFRD, filename + '_MCallVariantsFRD' + fileext);
          SaveFromMemo(MCallVariantsRAD, filename + '_MCallVariantsRAD' + fileext);
          SaveFromMemo(MCallVariantsOV, filename + '_MCallVariantsOV' + fileext);
          SaveFromMemo(MAddNotes, filename + '_MAddNotes' + fileext);
        end
      else
        ShowMessage('Please enter a longer file name.');
    end;
end;

procedure TFMain.BImportAllClick(Sender: TObject);
var varsl: TStringList;
    filename, fileext: string;
    filenamelength: Integer;
begin
  ODGeneral.Filter := 'Main Central Control Files (*.mcc)|*.mcc|All Files (*.*)|*.*';

  if ODGeneral.Execute then
    begin
      filename := ODGeneral.FileName;
      filenamelength := Length(filename);
      if filenamelength > 3 then
        begin
          if filename[filenamelength] = '.' then
            filename := copy(filename, 1, filenamelength-1)
          else
            if filename[filenamelength-1] = '.' then
              filename := copy(filename, 1, filenamelength-2)
            else
              if filename[filenamelength-2] = '.' then
                filename := copy(filename, 1, filenamelength-3)
              else
                if filename[filenamelength-3] = '.' then
                  filename := copy(filename, 1, filenamelength-4);

          varsl := TStringList.Create;
          varsl.LoadFromFile(ODGeneral.FileName);

          CBReferenceDNAGraph.Checked := Boolean(StrtoInt(varsl.Strings[1]));
          ECreateDNALength.Text := varsl.Strings[2];
          ECreateDNAAlphabet.Text := varsl.Strings[3];
          EPreProcessReferenceHorizon.Text := varsl.Strings[4];
          ECreateReadsLength.Text := varsl.Strings[5];
          ECreateReadsAmount.Text := varsl.Strings[6];
          ECreateReadsMisProb.Text := varsl.Strings[7];
          EAlignReadsLength.Text := varsl.Strings[8];
          EAlignReadsD.Text := varsl.Strings[9];
          EAlignReadsK.Text := varsl.Strings[10];
          ECreateDNAEditsLike.Text := varsl.Strings[11];
          ECreateDNAEditsContLike.Text := varsl.Strings[12];
          ECreateDNAInsertsLike.Text := varsl.Strings[13];
          ECreateDNAInsertsContLike.Text := varsl.Strings[14];
          ECreateDNADeletionsLike.Text := varsl.Strings[15];
          ECreateDNADeletionsContLike.Text := varsl.Strings[16];
          CBReferenceDNAFormat.Text := varsl.Strings[17];
          RBReferenceDNAGraphSnips.Checked := Boolean(StrtoInt(varsl.Strings[18]));
          RBReferenceDNAGraphMultiSnips.Checked := Boolean(StrtoInt(varsl.Strings[19]));
          RBReferenceDNAGraphFullMultiSnips.Checked := Boolean(StrtoInt(varsl.Strings[20]));
          RBReferenceDNAGraphFull.Checked := Boolean(StrtoInt(varsl.Strings[21]));
          CBuseHashesNew.Checked := Boolean(StrtoInt(varsl.Strings[22]));
          CBuseHashes.Checked := Boolean(StrtoInt(varsl.Strings[23]));
          CBuseBWT.Checked := Boolean(StrtoInt(varsl.Strings[24]));
          CBusePigeonhole.Checked := Boolean(StrtoInt(varsl.Strings[25]));
          if (varsl.Count > 26) then
            CBuseBWTnew.Checked := Boolean(StrtoInt(varsl.Strings[26]));

          fileext := '.mcm';

          LoadIntoMemo(MReferenceDNA, filename + '_MReferenceDNA' + fileext);
          LoadIntoMemo(MPreProcessReferenceBWT, filename + '_MPreProcessReferenceBWT' + fileext);
          LoadIntoMemo(MPreProcessReferenceSA, filename + '_MPreProcessReferenceSA' + fileext);
          LoadIntoMemo(MPreProcessReference, filename + '_MPreProcessReference' + fileext);
          LoadIntoMemo(MPreProcessReferenceOutRef, filename + '_MPreProcessReferenceOutRef' + fileext);
          LoadIntoMemo(MCreateDNAres, filename + '_MCreateDNAres' + fileext);
          LoadIntoMemo(MCreateReadsres, filename + '_MCreateReadsres' + fileext);
          LoadIntoMemo(MCreateReadspos, filename + '_MCreateReadspos' + fileext);
          LoadIntoMemo(MAlignReads, filename + '_MAlignReads' + fileext);
          LoadIntoMemo(MAssembleDNA, filename + '_MAssembleDNA' + fileext);
          LoadIntoMemo(MAssembleDNAShowAlign, filename + '_MAssembleDNAShowAlign' + fileext);
          LoadIntoMemo(MFillGaps, filename + '_MFillGaps' + fileext);
          LoadIntoMemo(MCallVariantsFRD, filename + '_MCallVariantsFRD' + fileext);
          LoadIntoMemo(MCallVariantsRAD, filename + '_MCallVariantsRAD' + fileext);
          LoadIntoMemo(MCallVariantsOV, filename + '_MCallVariantsOV' + fileext);
          LoadIntoMemo(MAddNotes, filename + '_MAddNotes' + fileext);

          showPreProcessReferenceOutRef := Boolean(StrtoInt(varsl.Strings[26]));

          varsl.Free;

          resetMemoHeights;
        end
      else
        ShowMessage('Please enter a longer file name.');
    end;
end;

procedure TFMain.SaveFromMemo(varm: TCustomMemo; filename: string);
var varsl: TStringList;
begin
  if varm is TRichEdit then
    begin
      varsl := TStringList.Create;
      varsl.Assign(varm.Lines);
      varsl.SaveToFile(filename);
      varsl.Free;
    end
  else
    varm.Lines.SaveToFile(filename);
end;

procedure TFMain.LoadIntoMemo(varm: TCustomMemo; filename: string);
begin
  if FileExists(filename) then
    varm.Lines.LoadFromFile(filename)
  else
    varm.Text := '';
end;

procedure TFMain.BAssembleDNAShowAlignClick(Sender: TObject);
var vars, readpos, indel, read, referenceDNA: string;
    i, j, position, anchor, indelat, referenceDNAlength: Integer;
begin
  referenceDNA := MReferenceDNA.Lines.Strings[0];

  if (not (CBuseBWT.Checked or CBuseBWTnew.Checked)) and showPreProcessReferenceOutRef then
    referenceDNA := MPreProcessReferenceOutRef.Lines.Strings[0];

  referenceDNAlength := Length(referenceDNA) - 1;

  vars := '';

  if referenceDNAlength > 99 then
    begin
      for i := 0 to referenceDNAlength do
        if i mod 100 = 0 then
          vars := vars + InttoStr((i div 100) mod 10)
        else
          vars := vars + ' ';
      vars := vars + #13#10;
    end;

  if referenceDNAlength > 9 then
    begin
      for i := 0 to referenceDNAlength do
        if i mod 10 = 0 then
          vars := vars + InttoStr((i div 10) mod 10)
        else
          vars := vars + ' ';
      vars := vars + #13#10;
    end;

  for i := 0 to referenceDNAlength do
    vars := vars + InttoStr(i mod 10);

  vars := vars + #13#10 +
  referenceDNA + ' (reference DNA string)' + #13#10 +
  MAssembleDNA.Lines.Strings[0] + ' (assembled individual DNA)';

  for i := 0 to MCreateReadsres.Lines.Count - 1 do
    begin
      read := MCreateReadsres.Lines[i];
      readpos := MAlignReads.Lines[i];
      if Length(readpos) > 0 then
        begin
          position := StrtoInt(copy(readpos, 2, pos(',', readpos) - 2));
          indel := copy(readpos, pos(',', readpos) + 2, Length(readpos));
          anchor := StrtoInt(copy(indel, 1, pos(']', indel) - 1));

          if pos('[', indel) > 0 then
            begin
              indel := copy(indel, pos('[', indel) + 1, Length(indel));
              indel := copy(indel, 1, pos(']', indel) - 1) + ',';

              while pos(',', indel) > 0 do
                begin
                  indelat := StrtoInt(copy(indel, 8, pos(',', indel) - 8));

                  if indel[1] = 'i' then
                    begin
                      // insert
                      if anchor > indelat then
                        Dec(position);
                    end
                  else
                    begin
                      // deletion
                      read := copy(read, 1, indelat - 1) + copy(read, indelat + 1, Length(read));
                      if anchor > indelat then
                        Inc(position);
                    end;

                  indel := copy(indel, pos(',', indel) + 1, Length(indel));
                end;
            end
          else
            indel := '';

          vars := vars + #13#10;

          for j := 1 to position do
            vars := vars + ' ';

          vars := vars + read + ' (' + readpos + ')';
        end
      else
        vars := vars + #13#10;
    end;

  MAssembleDNAShowAlign.Text := vars;
end;

procedure TFMain.BCallVariantsExportClick(Sender: TObject);
begin
  SDGeneral.Filter := 'All Files (*.*)|*.*';

  if SDGeneral.Execute then
    MCallVariantsFRD.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.BAlignReadsImportClick(Sender: TObject);
begin
  ODGeneral.Filter := 'All Files (*.*)|*.*';

  if ODGeneral.Execute then
    MAlignReads.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BAssembleDNAImportClick(Sender: TObject);
begin
  ODGeneral.Filter := 'All Files (*.*)|*.*';

  if ODGeneral.Execute then
    MAssembleDNA.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BFillGapsImportClick(Sender: TObject);
begin
  ODGeneral.Filter := 'All Files (*.*)|*.*';

  if ODGeneral.Execute then
    MFillGaps.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BCallVariantsImportClick(Sender: TObject);
begin
  ODGeneral.Filter := 'All Files (*.*)|*.*';

  if ODGeneral.Execute then
    MCallVariantsFRD.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BCallVariantsClick(Sender: TObject);
var i, len: Integer;
    retFRD, retRAD, retOV, referenceDNA, assembledDNA, individualDNA, nextline: string;
begin
  referenceDNA := MReferenceDNA.Lines.Strings[0];
  if (not (CBuseBWT.Checked or CBuseBWTnew.Checked)) and showPreProcessReferenceOutRef then
    referenceDNA := MPreProcessReferenceOutRef.Lines.Strings[0];

  assembledDNA := MFillGaps.Lines.Strings[0];
  if assembledDNA = '' then
    assembledDNA := MAssembleDNA.Lines.Strings[0];

  individualDNA := MCreateDNAres.Lines.Strings[0];



  retFRD := '';

  MCallVariantsFRD.Text := retFRD;



  retRAD := '';
  len := min(Length(referenceDNA), Length(assembledDNA));
  for i := 0 to len do
    if not (referenceDNA[i] = assembledDNA[i]) then
      retRAD := retRAD + 'at ' + InttoStr(i) + ' ' + referenceDNA[i] + ' -> ' + assembledDNA[i] + #13#10;

  MCallVariantsRAD.Text := retRAD;

  // TODO :: this is just an override for now, until we have a python script to explicitly generate this
  retFRD := retRAD;
  MCallVariantsFRD.Text := retRAD;



  retOV := '';
  len := min(Length(referenceDNA), Length(individualDNA));
  for i := 0 to len do
    if not (referenceDNA[i] = individualDNA[i]) then
      begin
        nextline := 'at ' + InttoStr(i) + ' ' + referenceDNA[i] + ' -> ' + individualDNA[i];
        retOV := retOV + nextline;

        if pos(nextline + #13#10, retFRD) > 0 then
          retOV := retOV + ' (found)'
        else
          retOV := retOV + ' (missing)';

        retOV := retOV + #13#10;
      end;

  MCallVariantsOV.Text := retOV;
end;

end.

{

  === RANDOM IDEAS ===

  :: What about this: run a second cycle, completely after you are done, and try to align the reads that are left over not to the reference, but to the string that has been created so far! SERIOUSLY! INSTAWIN! =)
     :: manually this can be done by putting the filled in string back as reference into step 1, and then going through steps 5 and 6 again. This should increase the overall rating in step 6, but most likely not most, sadly... ._.
     :: however, this can actually be improved: instead of re-aligning everything, delete all the reads except for the ones which got unknown positions (ofc we don't know if some reads were wrong, but we DO know if some got unknown results) and then, JUST FOR THESE FEW READS, hard-increase d by one, which will be very slow, but for only a handful of reads that should hopefully be okay, and then reassemble them and use their assembly to overwrite the existing reference (or well, create a string with very many gaps and then do gap-fill with the results we had already before or whatever)



  === TODO ===

  :: try out mismatch probability higher than 0 for reads themselves!
  :: use an actual net, not just a string
  :: when creating and aligning reads and so on, keep in mind that we should indeed look at the individual string plus its reverse opposite!
  :: create reads from individual string even with indels, maybe? (check if the data actually has indels in the reads themselves, or if the data usually only comes with mismatches, but not with indels!)
  :: find a good way to guesstime the size of the original string (currently the exact size of the reference string is assumed, but that won't fly once we are using a net...)
  :: use indels on the individual string (the source code is there, just commented out right now because it was too hard for us to pull off...)



  === FORMAT SPECIFICATIONS ===

  [STPU]
  :: one long sequence as plain text without any comments, newline characters or other whitespaces

  [FASTA]
  :: a file consisting of one or more sequences
  :: the first line starts with '>' or ';', immediately followed by the title of the sequence and then further commentary
  :: every other line starting with ';' is ignored
  :: all lines following the first line will be concatenated to form the sequence, until:
     ::: another line starting with a '>' is encountered OR
     ::: the line that was just read ends with a '*' OR
     ::: the file ends
  :: each sequence line of the file should ideally be 70 characters long
  :: each line (including comments) REALLY should not be more than 80 characters long

  [FASTG]
  (see FASTG specification)

  [GFA]
  (see Heng Li's first and second blog post)

}

