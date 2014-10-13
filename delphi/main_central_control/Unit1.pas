unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellApi, ExtCtrls, registry, Menus, MoyaUtils, Math;

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
    MCreateDNAres: TMemo;
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
    BPreProcessReference: TButton;
    MAlignReads: TMemo;
    BAlignReads: TButton;
    MAssembleDNA: TMemo;
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
    MPreProcessReference: TMemo;
    BPreProcessReferenceImport: TButton;
    BPreProcessReferenceExport: TButton;
    LPreProcessReferenceHorizonBefore: TLabel;
    EPreProcessReferenceHorizon: TEdit;
    LPreProcessReferenceHorizonAfter: TLabel;
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
    MFillGaps: TMemo;
    BFillGaps: TButton;
    BFillGapsExport: TButton;
    LFillGapsTop: TLabel;
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
  private
    function copyend(str, substr: string; depth: Integer): string;
    function createRandomDNAString: string;
    function python_path_set: Boolean;
    procedure prechange;
    procedure postchange;
    procedure CallbackCreateReadsCreated;
    procedure CallbackPreProcessReference;
    procedure CallbackAlignReads;
    procedure CallbackAssembleDNA;
    procedure RefreshAssembleDNAStats;
    procedure RefreshCreateDNAStats;
    procedure RefreshCreateReadsStats;
    procedure RefreshAlignReadsStats;
    procedure RefreshFillGapsStats;
    procedure SaveDAB;
    procedure LoadDAB;
    procedure runInPython(folder, input: string; callOnFinish: TCallbackMethod);
  public
    procedure SaveOptions;
    procedure ResetTo(difficulty: Integer);
  end;

const
  version = '0001';
  verdate = '8. 10. 2014';

var
  FMain: TFMain;
  Path, MainPath, PythonPath, TCheckExternal_FilePath: string;
  TCheckExternal_Call: TCallbackMethod;

implementation

uses Unit2;

{$R *.dfm}

procedure TFMain.FormResize(Sender: TObject);
begin
  PMain.Width := SBMain.ClientWidth - 1;

  GBCreateDNA.Width := PMain.ClientWidth - (2 * GBCreateDNA.Left);
  GBCreateReads.Width := GBCreateDNA.Width;
  GBReferenceDNA.Width := GBCreateDNA.Width;
  GBPreProcessReference.Width := GBCreateDNA.Width;
  GBAlignReads.Width := GBCreateDNA.Width;
  GBAssembleDNA.Width := GBCreateDNA.Width;
  GBFillGaps.Width := GBCreateDNA.Width;

  MCreateDNAres.Width := GBCreateDNA.ClientWidth - (2 * MCreateDNAres.Left);
  BCreateDNAImport.Width := (GBCreateDNA.Width - (4 * BCreateDNACreate.Left)) div 3;
  BCreateDNACreate.Width := BCreateDNAImport.Width;
  BCreateDNAExport.Width := BCreateDNAImport.Width;
  BCreateDNAImport.Left := BCreateDNACreate.Width + (BCreateDNACreate.Left * 2);
  BCreateDNAExport.Left := GBCreateDNA.ClientWidth - (BCreateDNACreate.Left + BCreateDNAExport.Width);

  LCreateDNAAlphabet.Left := (GBCreateDNA.Width + BCreateDNACreate.Left) div 2;
  ECreateDNAAlphabet.Left := LCreateDNAAlphabet.Left + LCreateDNAAlphabet.Width + 4;
  ECreateDNAAlphabet.Width := GBCreateDNA.ClientWidth - (BCreateDNACreate.Left + ECreateDNAAlphabet.Left);
  LCreateDNALengthBasepairs.Left := LCreateDNAAlphabet.Left - (LCreateDNALengthBasepairs.Width + LCreateDNALength.Left);
  ECreateDNALength.Left := LCreateDNALength.Width + LCreateDNALength.Left + 4;
  ECreateDNALength.Width := LCreateDNALengthBasepairs.Left - (ECreateDNALength.Left + 4);

  BOptions.Width := (GBReferenceDNA.Width + (2 * GBReferenceDNA.Left) - (3 * BOptions.Left)) div 2;
  BClose.Width := BOptions.Width;
  BClose.Left := GBReferenceDNA.Width + GBReferenceDNA.Left - BClose.Width;

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
  MCreateReadsres.Width := (GBCreateReads.ClientWidth - (3 * MCreateReadsres.Left)) div 2;
  MCreateReadspos.Width := MCreateReadsres.Width;
  MCreateReadspos.Left := GBCreateReads.ClientWidth - (MCreateReadsres.Left + MCreateReadspos.Width);
  LCreateReadsMpos.Left := MCreateReadspos.Left;

  BCreateReadsImport.Width := (GBCreateReads.Width - (4 * BCreateReadsCreate.Left)) div 3;
  BCreateReadsCreate.Width := BCreateReadsImport.Width;
  BCreateReadsExport.Width := BCreateReadsImport.Width;
  BCreateReadsImport.Left := BCreateReadsCreate.Width + (BCreateReadsCreate.Left * 2);
  BCreateReadsExport.Left := GBCreateReads.ClientWidth - (BCreateReadsCreate.Left + BCreateReadsExport.Width);

  BReferenceDNAImport.Width := (GBReferenceDNA.ClientWidth - (5 * MReferenceDNA.Left)) div 4;
  CBReferenceDNAGraph.Width := BReferenceDNAImport.Width;
  BReferenceDNACreate.Width := BReferenceDNAImport.Width;
  BReferenceDNAExport.Width := BReferenceDNAImport.Width;
  BReferenceDNAImport.Left := (GBReferenceDNA.ClientWidth + MReferenceDNA.Left) div 2;
  BReferenceDNAExport.Left := GBReferenceDNA.ClientWidth - (BReferenceDNAExport.Width + MReferenceDNA.Left);
  BReferenceDNACreate.Left := (MReferenceDNA.Left * 2) + CBReferenceDNAGraph.Width;
  MReferenceDNA.Width := GBReferenceDNA.ClientWidth - (2 * MReferenceDNA.Left);

  BPreProcessReferenceImport.Width := (GBPreProcessReference.ClientWidth - (5 * MPreProcessReference.Left)) div 4;
  BPreProcessReference.Width := BPreProcessReferenceImport.Width;
  BPreProcessReferenceExport.Width := BPreProcessReferenceImport.Width;
  BPreProcessReferenceImport.Left := (GBPreProcessReference.ClientWidth + MPreProcessReference.Left) div 2;
  BPreProcessReferenceExport.Left := GBPreProcessReference.ClientWidth - (BPreProcessReferenceExport.Width + MPreProcessReference.Left);
  BPreProcessReference.Left := (MPreProcessReference.Left * 2) + BPreProcessReference.Width;
  LPreProcessReferenceHorizonAfter.Left := BPreProcessReference.Left - (LPreProcessReferenceHorizonAfter.Width + MPreProcessReference.Left);
  EPreProcessReferenceHorizon.Left := LPreProcessReferenceHorizonBefore.Left + LPreProcessReferenceHorizonBefore.Width + 4;
  EPreProcessReferenceHorizon.Width := LPreProcessReferenceHorizonAfter.Left - (4 + EPreProcessReferenceHorizon.Left);
  MPreProcessReference.Width := GBPreProcessReference.Width - (2 * MPreProcessReference.Left);

  BAlignReads.Width := (GBAlignReads.Width - (3 * BAlignReads.Left)) div 2;
  BAlignReadsExport.Width := BAlignReads.Width;
  BAlignReadsExport.Left := GBAlignReads.Width - (BAlignReads.Left + BAlignReadsExport.Width);
  MAlignReads.Width := GBAlignReads.Width - (2 * BAlignReads.Left);
  LAlignReadsDBefore.Left := BAlignReadsExport.Left;
  EAlignReadsD.Left := LAlignReadsDBefore.Left + LAlignReadsDBefore.Width + 4;
  LAlignReadsDAfter.Left := GBAlignReads.Width - (BAlignReads.Left + LAlignReadsDAfter.Width);
  EAlignReadsD.Width := LAlignReadsDAfter.Left - (4 + EAlignReadsD.Left);
  EAlignReadsLength.Left := LAlignReadsLengthBefore.Left + LAlignReadsLengthBefore.Width + 4;
  LAlignReadsLengthAfter.Left := BAlignReads.Width + BAlignReads.Left - LAlignReadsDAfter.Width;
  EAlignReadsLength.Width := LAlignReadsLengthAfter.Left - (4 + EAlignReadsLength.Left);

  BAssembleDNA.Width := (GBAssembleDNA.Width - (3 * BAssembleDNA.Left)) div 2;
  BAssembleDNAExport.Width := BAssembleDNA.Width;
  BAssembleDNAExport.Left := GBAssembleDNA.Width - (BAssembleDNA.Left + BAssembleDNAExport.Width);
  MAssembleDNA.Width := GBAssembleDNA.Width - (2 * BAssembleDNA.Left);

  BFillGaps.Width := (GBFillGaps.Width - (3 * BFillGaps.Left)) div 2;
  BFillGapsExport.Width := BFillGaps.Width;
  BFillGapsExport.Left := GBFillGaps.Width - (BFillGaps.Left + BFillGapsExport.Width);
  MFillGaps.Width := GBFillGaps.Width - (2 * BFillGaps.Left);

  SBAllMemos.Left := MAssembleDNA.Left + GBAssembleDNA.Left;
  SBAllMemos.Width := MAssembleDNA.Width;
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
var reference, res, alphabet: string;
    len, alen, i, edits, insertions, deletions: Integer;
begin
  res := '';
  reference := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);
  if Length(reference) = 0 then
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

  while i < len do
    begin            
      // TODO :: make the whole alignment work even with deletions
      {                                                         
      // deletions
      if Random(1000) < 10 then
      begin
      while (Random(1000) < 200) and (i < len) do
      begin
      Inc(i);
      Inc(deletions);
      end;
      end;
      }

      // TODO :: make the whole alignment work even with insertions
      {                                                    
      // insertions
      if Random(1000) < 10 then
      begin
      while Random(1000) < 200 do
      begin
      Inc(insertions);
      res := res + alphabet[Random(alen)+1];
      end;
      end;
      }

      // edits
      if Random(1000) < 50 then
        begin
          Inc(i);
          Inc(edits);
          res := res + alphabet[Random(alen)+1];
          while (Random(1000) < 200) and (i < len) do
            begin
              Inc(i);
              Inc(edits);
              res := res + alphabet[Random(alen)+1];
            end;
        end
      else
        // no change
        begin
          Inc(i);
          res := res + reference[i];
        end;
    end;

  MCreateDNAres.Text := res;
  LCreateDNAStats.Caption := 'Status: Individual DNA string with ' + InttoStr(edits * 100 div len) + '% changes, ' + InttoStr(insertions * 100 div len) + '% insertions and ' + InttoStr(deletions * 100 div len) + '% deletions with regards to reference string';
end;

function TFMain.createRandomDNAString: string;
var i, len, alen: Integer;
    alphabet: string;
begin
  result := '';

  Randomize;

  len := StrtoInt(ECreateDNALength.Text);
  alphabet := ECreateDNAAlphabet.Text;
  if (Length(alphabet) = 0) then
    begin
      ShowMessage('Please specify an alphabet first.');
      exit;
    end;
  alen := Length(alphabet);

  for i := 1 to len do
    result := result + alphabet[Random(alen)+1];
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  Path := copyend(Application.Exename, '\', 1) + '\';
  MainPath := copyend(Application.Exename, '\', 3) + '\';
  ODGeneral.InitialDir := MainPath;
  SDGeneral.InitialDir := MainPath;
  LoadDAB;

  LVersion.Caption := 'Version: ' + version[1] + '.' + version[2] + '.' + version[3] + '.' + version[4] + ' by Moyaccercchi, 7. 10. 2014 .. ' + verdate;
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
var dnastring, lengthstring, amountstring, misprobstring, alphabetstring: string;
begin
  if python_path_set then
    begin
      MCreateReadsres.Text := '';
      MCreateReadspos.Text := '';

      dnastring := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);
      if Length(dnastring) = 0 then
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

      runInPython('4_read_generator', dnastring + #13#10 + lengthstring + #13#10 + amountstring + #13#10 + misprobstring + #13#10 + alphabetstring, FMain.CallbackCreateReadsCreated);
    end;
end;

procedure TFMain.CallbackCreateReadsCreated;
begin
  MCreateReadsres.Lines.LoadFromFile(MainPath + 'python\4_read_generator\out.txt');

  if FileExists(MainPath + 'python\4_read_generator\outpos.txt') then
    MCreateReadspos.Lines.LoadFromFile(MainPath + 'python\4_read_generator\outpos.txt')
  else
    MCreateReadspos.Text := '';
end;

procedure TFMain.CallbackPreProcessReference;
begin
  MPreProcessReference.Lines.LoadFromFile(MainPath + 'python\2_reference_preprocessor\out.txt');
end;

procedure TFMain.TCheckExternalsTimer(Sender: TObject);
begin
  if FileExists(TCheckExternal_FilePath) then
    begin
      TCheckExternals.Enabled := false;
      TCheckExternal_Call;
    end;
end;

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

  finally
    regist.free;
  end;
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
  finally
    regist.free;
  end;
end;

procedure TFMain.BOptionsClick(Sender: TObject);
begin
  FOptions.EPythonPath.Text := PythonPath;
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
  prechange;

  (Screen.ActiveControl as TMemo).SelectAll;
  (Screen.ActiveControl as TMemo).PasteFromClipboard;

  postchange;
end;

procedure TFMain.prechange;
begin
  if (Screen.ActiveControl = MPreProcessReference) then
    MPreProcessReference.ReadOnly := false;

  if (Screen.ActiveControl = MCreateReadsres) then
    MCreateReadsres.ReadOnly := false;

  if (Screen.ActiveControl = MAlignReads) then
    MAlignReads.ReadOnly := false;
end;

procedure TFMain.postchange;
begin
  if (Screen.ActiveControl = MPreProcessReference) then
    begin
      MPreProcessReference.Lines.SaveToFile(MainPath + 'python\2_reference_preprocessor\out.txt');
      MPreProcessReference.ReadOnly := true;
    end;

  if (Screen.ActiveControl = MCreateReadsres) then
    begin
      MCreateReadsres.Lines.SaveToFile(MainPath + 'python\4_read_generator\out.txt');
      MCreateReadsres.ReadOnly := true;
    end;

  if (Screen.ActiveControl = MAlignReads) then
    begin
      MAlignReads.Lines.SaveToFile(MainPath + 'python\5_read_aligner\out.txt');
      MAlignReads.ReadOnly := true;
    end;
end;

procedure TFMain.CopySelection1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).CopyToClipboard;
end;

procedure TFMain.PasteSelection1Click(Sender: TObject);
begin
  prechange;

  (Screen.ActiveControl as TMemo).PasteFromClipboard;

  postchange;
end;

procedure TFMain.Undo1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).Undo;
end;

procedure TFMain.BReferenceDNAImportClick(Sender: TObject);
begin
  if ODGeneral.Execute then
    MReferenceDNA.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BCreateDNAImportClick(Sender: TObject);
begin
  if ODGeneral.Execute then
    MCreateDNAres.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BCreateReadsImportClick(Sender: TObject);
begin
  if ODGeneral.Execute then
    begin
      MCreateReadsres.Text := LoadHugeFile(ODGeneral.FileName);
      MCreateReadsres.Lines.SaveToFile(MainPath + 'python\4_read_generator\out.txt');
    end;
end;

procedure TFMain.BReferenceDNAExportClick(Sender: TObject);
begin
  if SDGeneral.Execute then
    MReferenceDNA.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.BCreateDNAExportClick(Sender: TObject);
begin
  if SDGeneral.Execute then
    MCreateDNAres.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.BCreateReadsExportClick(Sender: TObject);
begin
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
  RefreshCreateDNAStats;

  CBReferenceDNAGraph.Checked := pos('%', MReferenceDNA.Text) > 0;

  SBAllMemos.Max := Length(MReferenceDNA.Text);
end;

procedure TFMain.RefreshCreateDNAStats;
var reference, individual: string;
begin
  reference := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);
  individual := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);

  if Length(individual) = 0 then
    LCreateDNAStats.Caption := 'Status: No individual DNA string present'
  else
    if Length(reference) = 0 then
      LCreateDNAStats.Caption := 'Status: Random individual DNA string, not based on any reference'
    else
      LCreateDNAStats.Caption := 'Status: Individual DNA string with ' + InttoStr(Length(individual) * 100 div Length(reference)) + '% length of DNA reference string';
end;

procedure TFMain.BPreProcessReferenceClick(Sender: TObject);
var dnastring, lengthstring: string;
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

      runInPython('2_reference_preprocessor', dnastring + #13#10 + lengthstring, FMain.CallbackPreProcessReference);
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

  output.SaveToFile(Path + 'batch.bat');
  output.Free;

  ShellExecute(Application.handle, PChar('open'), PChar('batch.bat'), PChar(''), PChar(Path), SW_SHOW);

  TCheckExternal_FilePath := OutFilePath;
  TCheckExternal_Call := callOnFinish;
  TCheckExternals.Enabled := true;
end;

procedure TFMain.BAlignReadsClick(Sender: TObject);
var reference: string;
begin
  if python_path_set then
    begin
      MAlignReads.Text := '';

      reference := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);
      if Length(reference) = 0 then
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

      runInPython('5_read_aligner', reference + #13#10 + MainPath + 'python\2_reference_preprocessor\out.txt' + #13#10 + MainPath + 'python\4_read_generator\out.txt' + #13#10 + EAlignReadsD.Text + #13#10 + EAlignReadsLength.Text, FMain.CallbackAlignReads);
    end;
end;

procedure TFMain.CallbackAlignReads;
begin
  MAlignReads.Lines.LoadFromFile(MainPath + 'python\5_read_aligner\out.txt');
end;

function TFMain.python_path_set: Boolean;
begin
  result := Length(PythonPath) > 0;

  if not result then
    ShowMessage('Please first go to the options page and specify a valid path for python.');
end;

procedure TFMain.BAssembleDNAClick(Sender: TObject);
var reference: string;
begin
  if python_path_set then
    begin
      MAssembleDNA.Text := '';

      reference := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);
      if Length(reference) = 0 then
        begin
          BAlignReadsClick(Sender);
          ShowMessage('Please try again after the alignment is done.');
          exit;
        end;

      runInPython('6_string_assembler', reference + #13#10 + MainPath + 'python\4_read_generator\out.txt' + #13#10 + MainPath + 'python\5_read_aligner\out.txt', FMain.CallbackAssembleDNA);
    end;
end;

procedure TFMain.CallbackAssembleDNA;
begin
  MAssembleDNA.Lines.LoadFromFile(MainPath + 'python\6_string_assembler\out.txt');

  RefreshAssembleDNAStats;
end;

procedure TFMain.SelectAll1Click(Sender: TObject);
begin
  (Screen.ActiveControl as TMemo).SelectAll;
end;

procedure TFMain.CutAll1Click(Sender: TObject);
begin
  prechange;

  (Screen.ActiveControl as TMemo).SelectAll;
  (Screen.ActiveControl as TMemo).CutToClipboard; 

  postchange;
end;

procedure TFMain.CutSelection1Click(Sender: TObject);
begin          
  prechange;

  (Screen.ActiveControl as TMemo).CutToClipboard;

  postchange;
end;

procedure TFMain.BAlignReadsExportClick(Sender: TObject);
begin
  if SDGeneral.Execute then
    MAlignReads.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.BAssembleDNAExportClick(Sender: TObject);
begin
  if SDGeneral.Execute then
    MAssembleDNA.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.BPreProcessReferenceImportClick(Sender: TObject);
begin
  if ODGeneral.Execute then
    begin
      MPreProcessReference.Text := LoadHugeFile(ODGeneral.FileName);
      MPreProcessReference.Lines.SaveToFile(MainPath + 'python\2_reference_preprocessor\out.txt');
    end;
end;

procedure TFMain.BPreProcessReferenceExportClick(Sender: TObject);
begin
  if SDGeneral.Execute then
    MPreProcessReference.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.MAssembleDNAChange(Sender: TObject);
begin
  RefreshAssembleDNAStats;

  SBAllMemos.Max := Length(MAssembleDNA.Text);
end;

procedure TFMain.RefreshAssembleDNAStats;
var res, original, assembledDNA: string;
    i, correct, noteventried, minlen: Integer;
begin
  res := 'Status: ';

  original := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);
  assembledDNA := StringReplace(MAssembleDNA.Text, #13#10, '', [rfReplaceAll]);

  if Length(assembledDNA) = 0 then
    res := res + 'Assembly not initialized'
  else
    if Length(original) = 0 then
      res := res + 'No individual DNA string present'
    else
      begin
        correct := 0;
        noteventried := 0;
        for i := 1 to min(Length(assembledDNA), Length(original)) do
          if assembledDNA[i] = original[i] then
            Inc(correct)
          else
            if assembledDNA[i] = '_' then
              Inc(noteventried);
        minlen := min(Length(assembledDNA), Length(original));
        res := res + 'Assembly to ' + InttoStr((correct * 100) div minlen) + '% correct, to ' + InttoStr((noteventried * 100) div minlen) + '% stated as unknown and to ' + InttoStr(100 - (((correct + noteventried) * 100) div minlen)) + '% stated wrongly.';
      end;

  LAssembleDNAStatus.Caption := res;
end;

procedure TFMain.EPreProcessReferenceHorizonChange(Sender: TObject);
begin
  if not (EPreProcessReferenceHorizon.Text = EAlignReadsLength.Text) then
    EAlignReadsLength.Text := EPreProcessReferenceHorizon.Text;
end;

procedure TFMain.MCreateReadsresChange(Sender: TObject);
begin
  RefreshCreateReadsStats;
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
  original := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);

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
var res: string;
    Readspos, Alignedreads: TStringList;
    fulllength, correctlyaligned, noteventried, i: Integer;
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

        for i := 0 to fulllength do
          if Readspos[i] = Alignedreads[i] then
            Inc(correctlyaligned)
          else
            if Alignedreads[i] = '' then
              Inc(noteventried);

        res := res + 'Alignment to ' + InttoStr((correctlyaligned * 100) div (fulllength + 1)) + '% correct, to ' + InttoStr((noteventried * 100) div (fulllength + 1)) + '% stated as unknown and to ' + InttoStr(100 - (((correctlyaligned + noteventried) * 100) div (fulllength + 1))) + '% stated wrongly.';
      end;

  Readspos.Free;
  Alignedreads.Free;

  LAlignReadsStatus.Caption := res;
end;

procedure TFMain.MAlignReadsChange(Sender: TObject);
begin
  RefreshAlignReadsStats;
end;

procedure TFMain.EAlignReadsLengthChange(Sender: TObject);
begin
  if not (EPreProcessReferenceHorizon.Text = EAlignReadsLength.Text) then
    EPreProcessReferenceHorizon.Text := EAlignReadsLength.Text;
end;

procedure TFMain.SBAllMemosScroll(Sender: TObject; ScrollCode: TScrollCode;
var ScrollPos: Integer);
begin
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
end;

procedure TFMain.ResetTo(difficulty: Integer);
begin
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
    end;
  end;

  SaveDAB;
end;

procedure TFMain.BFillGapsExportClick(Sender: TObject);
begin
  if SDGeneral.Execute then
    MFillGaps.Lines.SaveToFile(SDGeneral.FileName);
end;

procedure TFMain.MFillGapsChange(Sender: TObject);
begin
  RefreshFillGapsStats;

  SBAllMemos.Max := Length(MFillGaps.Text);
end;

procedure TFMain.RefreshFillGapsStats;
var res, original, assembledDNA: string;
    i, correct, noteventried, minlen: Integer;
begin
  res := 'Status: ';

  original := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);
  assembledDNA := StringReplace(MFillGaps.Text, #13#10, '', [rfReplaceAll]);

  if Length(assembledDNA) = 0 then
    res := res + 'Assembly not initialized'
  else
    if Length(original) = 0 then
      res := res + 'No individual DNA string present'
    else
      begin
        correct := 0;
        noteventried := 0;
        for i := 1 to min(Length(assembledDNA), Length(original)) do
          if assembledDNA[i] = original[i] then
            Inc(correct)
          else
            if assembledDNA[i] = '_' then
              Inc(noteventried);
        minlen := min(Length(assembledDNA), Length(original));
        res := res + 'Assembly to ' + InttoStr((correct * 100) div minlen) + '% correct, to ' + InttoStr((noteventried * 100) div minlen) + '% stated as unknown and to ' + InttoStr(100 - (((correct + noteventried) * 100) div minlen)) + '% stated wrongly.';
      end;

  LFillGapsStatus.Caption := res;
end;

procedure TFMain.BFillGapsClick(Sender: TObject);
var assembledDNA, reference: string;
    i, minlen: Integer;
begin
  assembledDNA := StringReplace(MAssembleDNA.Text, #13#10, '', [rfReplaceAll]);
  reference := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);

  minlen := min(Length(assembledDNA), Length(reference));

  for i := 1 to minlen do
    if assembledDNA[i] = '_' then
      assembledDNA[i] := reference[i];

  MFillGaps.Text := assembledDNA;
end;

end.

{
  === DA IMMY ===
  :: What about this: run a second cycle, completely after you are done, and try to align the reads that are left over not to the reference, but to the string that has been created so far! SERIOUSLY! INSTAWIN! =)
     :: manually this can be done by putting the filled in string back as reference into step 1, and then going through steps 5 and 6 again. This should increase the overall rating in step 6, but most likely not most, sadly... ._.
     
  === TODO ===
  :: try out mismatch probability higher than 0 for reads themselves!
  :: use an actual net, not just a string
  :: when creating and aligning reads and so on, keep in mind that we should indeed look at the individual string plus its reverse opposite!
  :: create reads from individual string even with indels, maybe? (check if the data actually has indels in the reads themselves, or if the data usually only comes with mismatches, but not with indels!)
  :: find a good way to guesstime the size of the original string (currently the exact size of the reference string is assumed, but that won't fly once we are using a net...)
  :: use indels on the individual string (the source code is there, just commented out right now because it was too hard for us to pull off...)
}

