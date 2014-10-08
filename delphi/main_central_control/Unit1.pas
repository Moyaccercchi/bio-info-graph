unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellApi, ExtCtrls, registry, Menus, MoyaUtils;

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
    Label1: TLabel;
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
  private
    function copyend(str, substr: string; depth: Integer): string;
    function createRandomDNAString: string;
    procedure CallbackCreateReadsCreated;
    procedure CallbackPreProcessReference;
    procedure SaveDAB;
    procedure LoadDAB;
    procedure RefreshCreateDNAStats;
    procedure soznotreadyyet;
    procedure runInPython(folder, input: string; callOnFinish: TCallbackMethod);
  public
    procedure SaveOptions;
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

  BOptions.Width := (FMain.ClientWidth - (3 * BOptions.Left)) div 2;
  BClose.Width := BOptions.Width;

  BClose.Left := FMain.ClientWidth - (GBReferenceDNA.Left + BClose.Width);

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
  MCreateReadsres.Width := GBCreateReads.ClientWidth - (2 * MCreateReadsres.Left);

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
  BReferenceDNACreate.Left := (CBReferenceDNAGraph.Left * 2) + CBReferenceDNAGraph.Width;
  MReferenceDNA.Width := GBReferenceDNA.ClientWidth - (2 * MReferenceDNA.Left);

  BPreProcessReferenceImport.Width := (GBPreProcessReference.Width - (4 * BPreProcessReference.Left)) div 3;
  BPreProcessReference.Width := BPreProcessReferenceImport.Width;
  BPreProcessReferenceExport.Width := BPreProcessReferenceImport.Width;
  BPreProcessReferenceImport.Left := BPreProcessReference.Width + (BPreProcessReference.Left * 2);
  BPreProcessReferenceExport.Left := GBPreProcessReference.ClientWidth - (BPreProcessReference.Left + BPreProcessReferenceExport.Width);
  MPreProcessReference.Width := GBPreProcessReference.Width - (2 * MPreProcessReference.Left);

  BAlignReads.Width := (GBAlignReads.Width - (3 * BAlignReads.Left)) div 2;
  BAlignReadsExport.Width := BAlignReads.Width;
  BAlignReadsExport.Left := GBAlignReads.Width - (BAlignReads.Left + BAlignReadsExport.Width);
  MAlignReads.Width := GBAlignReads.Width - (2 * BAlignReads.Left);

  BAssembleDNA.Width := (GBAssembleDNA.Width - (3 * BAssembleDNA.Left)) div 2;
  BAssembleDNAExport.Width := BAssembleDNA.Width;
  BAssembleDNAExport.Left := GBAssembleDNA.Width - (BAssembleDNA.Left + BAssembleDNAExport.Width);
  MAssembleDNA.Width := GBAssembleDNA.Width - (2 * BAssembleDNA.Left);
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
      // deletions
      if Random(1000) < 10 then
        begin
          while (Random(1000) < 500) and (i < len) do
            begin
              Inc(i);
              Inc(deletions);
            end;
        end;

      // insertions
      if Random(1000) < 10 then
        begin
          while Random(1000) < 500 do
            begin
              Inc(insertions);
              res := res + alphabet[Random(alen)+1];
            end;
        end;

      // edits
      if Random(1000) < 150 then
        begin
          while (Random(1000) < 500) and (i < len) do
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
  if Length(PythonPath) = 0 then
    begin
      ShowMessage('Please first go to the options page and specify a valid path for python.');
      exit;
    end;

  MCreateReadsres.Text := '';

  dnastring := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);
  if Length(dnastring) = 0 then
    begin
      if (Sender = nil) then
        ShowMessage('No DNA string can be generated.')
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

procedure TFMain.CallbackCreateReadsCreated;
begin
  MCreateReadsres.Lines.LoadFromFile(MainPath + 'python\4_read_generator\out.txt');
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
    MCreateReadsres.Text := LoadHugeFile(ODGeneral.FileName);
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
end;

procedure TFMain.MReferenceDNAChange(Sender: TObject);
begin
  RefreshCreateDNAStats;

  CBReferenceDNAGraph.Checked := pos('%', MReferenceDNA.Text) > 0;
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
  if Length(PythonPath) = 0 then
    begin
      ShowMessage('Please first go to the options page and specify a valid path for python.');
      exit;
    end;

  MPreProcessReference.Text := '';

  dnastring := StringReplace(MReferenceDNA.Text, #13#10, '', [rfReplaceAll]);
  if Length(dnastring) = 0 then
    begin
      if (Sender = nil) then
        ShowMessage('No DNA string can be generated.')
      else
        begin
          BReferenceDNACreateClick(Sender);
          BPreProcessReferenceClick(nil);
        end;
      exit;
    end;
  lengthstring := ECreateReadsLength.Text;

  runInPython('2_reference_preprocessor', dnastring + #13#10 + lengthstring, FMain.CallbackPreProcessReference);
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

procedure TFMain.soznotreadyyet;
begin
  ShowMessage('Sorry, I''m not ready yet.');
end;

procedure TFMain.BAlignReadsClick(Sender: TObject);
begin
  soznotreadyyet;
end;

procedure TFMain.BAssembleDNAClick(Sender: TObject);
begin
  soznotreadyyet;
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
    MPreProcessReference.Text := LoadHugeFile(ODGeneral.FileName);
end;

procedure TFMain.BPreProcessReferenceExportClick(Sender: TObject);
begin
  if SDGeneral.Execute then
    MPreProcessReference.Lines.SaveToFile(SDGeneral.FileName);
end;

end.

