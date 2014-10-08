unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellApi, ExtCtrls, registry;

type
  TFMain = class(TForm)
    GBCreateDNA: TGroupBox;
    BClose: TButton;
    GBCreateReads: TGroupBox;
    GBReconstructDNA: TGroupBox;
    LCreateDNALength: TLabel;
    ECreateDNALength: TEdit;
    LCreateDNALengthBasepairs: TLabel;
    BCreateDNACreate: TButton;
    BCreateDNACopy: TButton;
    MCreateDNAres: TMemo;
    LCreateDNAAlphabet: TLabel;
    ECreateDNAAlphabet: TEdit;
    BOptions: TButton;
    BGBCreateReadsCreate: TButton;
    ECreateReadsLengthBefore: TLabel;
    ECreateReadsAmount: TEdit;
    ECreateReadsLengthAfter: TLabel;
    ECreateReadsAmountBefore: TLabel;
    ECreateReadsLength: TEdit;
    ECreateReadsMisProbBefore: TLabel;
    ECreateReadsMisProb: TEdit;
    ECreateReadsMisProbAfter: TLabel;
    TCheckExternals: TTimer;
    MCreateReadsres: TMemo;
    CBReconstructDNAReference: TCheckBox;
    MReconstructDNAReference: TMemo;
    BReconstructDNACreateReference: TButton;
    BCreateDNACreateBasedOnRef: TButton;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure BCreateDNACopyClick(Sender: TObject);
    procedure BCreateDNACreateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BGBCreateReadsCreateClick(Sender: TObject);
    procedure TCheckExternalsTimer(Sender: TObject);
    procedure BOptionsClick(Sender: TObject);
    procedure BCreateDNACreateBasedOnRefClick(Sender: TObject);
    procedure BReconstructDNACreateReferenceClick(Sender: TObject);
  private
    function copyend(str, substr: string; depth: Integer): string;
    procedure BGBCreateReadsCreated;
    procedure SaveDAB;
    procedure LoadDAB;
  public
    procedure SaveOptions;
  end;

const
  version = '0001';

var
  FMain: TFMain;
  Path, MainPath, PythonPath, TCheckExternal_FilePath: string;
  TCheckExternal_Call: procedure of object;

implementation

uses Unit2;

{$R *.dfm}

procedure TFMain.FormResize(Sender: TObject);
begin
  GBCreateDNA.Width := FMain.ClientWidth - (2 * GBCreateDNA.Left);
  GBCreateReads.Width := GBCreateDNA.Width;
  GBReconstructDNA.Width := GBCreateDNA.Width;

  MCreateDNAres.Width := GBCreateDNA.ClientWidth - (2 * MCreateDNAres.Left);
  BCreateDNACreateBasedOnRef.Width := (GBCreateDNA.Width - (4 * BCreateDNACreate.Left)) div 3;
  BCreateDNACreate.Width := BCreateDNACreateBasedOnRef.Width;
  BCreateDNACopy.Width := BCreateDNACreateBasedOnRef.Width;
  BCreateDNACreateBasedOnRef.Left := BCreateDNACreate.Width + (BCreateDNACreate.Left * 2);
  BCreateDNACopy.Left := GBCreateDNA.ClientWidth - (BCreateDNACreate.Left + BCreateDNACopy.Width);

  LCreateDNAAlphabet.Left := (GBCreateDNA.Width - (3 * BCreateDNACreate.Left)) div 2;
  ECreateDNAAlphabet.Left := LCreateDNAAlphabet.Left + LCreateDNAAlphabet.Width + 4;
  ECreateDNAAlphabet.Width := GBCreateDNA.ClientWidth - (BCreateDNACreate.Left + ECreateDNAAlphabet.Left);
  LCreateDNALengthBasepairs.Left := LCreateDNAAlphabet.Left - (LCreateDNALengthBasepairs.Width + LCreateDNALength.Left);
  ECreateDNALength.Left := LCreateDNALength.Width + LCreateDNALength.Left + 4;
  ECreateDNALength.Width := LCreateDNALengthBasepairs.Left - (ECreateDNALength.Left + 4);

  BOptions.Width := (FMain.ClientWidth - (3 * BOptions.Left)) div 2;
  BClose.Width := BOptions.Width;

  BClose.Left := FMain.ClientWidth - (GBCreateDNA.Left + BClose.Width);
  BClose.Top := FMain.ClientHeight - (GBCreateDNA.Top + BClose.Height);
  BOptions.Top := BClose.Top;

  ECreateReadsAmountBefore.Left := (GBCreateReads.ClientWidth - (4 * ECreateReadsLengthBefore.Left)) div 3;
  ECreateReadsMisProbBefore.Left := (2 * (GBCreateReads.ClientWidth - (4 * ECreateReadsLengthBefore.Left))) div 3;
  ECreateReadsMisProbAfter.Left := GBCreateReads.ClientWidth - (ECreateReadsMisProbAfter.Width + ECreateReadsLengthBefore.Left);
  ECreateReadsMisProb.Left := ECreateReadsMisProbBefore.Left + ECreateReadsMisProbBefore.Width + 4;
  ECreateReadsMisProb.Width := ECreateReadsMisProbAfter.Left - (ECreateReadsMisProb.Left + 4);
  ECreateReadsAmount.Left := ECreateReadsAmountBefore.Left + ECreateReadsAmountBefore.Width + 4;
  ECreateReadsAmount.Width := ECreateReadsMisProbBefore.Left - (ECreateReadsAmount.Left + ECreateReadsLengthBefore.Left);
  ECreateReadsLengthAfter.Left := ECreateReadsAmountBefore.Left - (ECreateReadsLengthAfter.Width + ECreateReadsLengthBefore.Left);
  ECreateReadsLength.Left := ECreateReadsLengthBefore.Left + ECreateReadsLengthBefore.Width + 4;
  ECreateReadsLength.Width := ECreateReadsLengthAfter.Left - (ECreateReadsLength.Left + 4);
  BGBCreateReadsCreate.Width := GBCreateReads.ClientWidth - (2 * BGBCreateReadsCreate.Left);
  MCreateReadsres.Width := GBCreateReads.ClientWidth - (2 * MCreateReadsres.Left);

  BReconstructDNACreateReference.Left := GBReconstructDNA.ClientWidth - (BReconstructDNACreateReference.Width + MReconstructDNAReference.Left);
  MReconstructDNAReference.Width := GBReconstructDNA.ClientWidth - (2 * MReconstructDNAReference.Left);
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

procedure TFMain.BCreateDNACopyClick(Sender: TObject);
begin
  MCreateDNAres.SelectAll;
  MCreateDNAres.CopyToClipboard;
end;

procedure TFMain.BCreateDNACreateClick(Sender: TObject);
var i, len, alen: Integer;
    alphabet, res: string;
begin
  Randomize;

  len := StrtoInt(ECreateDNALength.Text);
  alphabet := ECreateDNAAlphabet.Text;
  alen := Length(alphabet);
  res := '';

  for i := 1 to len do
    begin
      res := res + alphabet[Random(alen)+1];
    end;

  MCreateDNAres.Text := res;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  Path := copyend(Application.Exename, '\', 1) + '\';
  MainPath := copyend(Application.Exename, '\', 3) + '\';
  LoadDAB;
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

procedure TFMain.BGBCreateReadsCreateClick(Sender: TObject);
var output: TStringList;
    dnastring, lengthstring, amountstring, misprobstring, alphabetstring, FilePath, InFilePath, OutFilePath: string;
begin
  MCreateReadsres.Text := '';

  output := TStringList.Create;

  dnastring := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);
  if Length(dnastring) = 0 then
    begin
      if (Sender = nil) then
        ShowMessage('No DNA string can be generated.')
      else
        begin
          BCreateDNACreateClick(nil);
          BGBCreateReadsCreateClick(nil);
        end;
      exit;
    end;
  lengthstring := ECreateReadsLength.Text;
  amountstring := ECreateReadsAmount.Text;
  misprobstring := ECreateReadsMisProb.Text;
  alphabetstring := ECreateDNAAlphabet.Text;

  output.Text := dnastring + #13#10 + lengthstring + #13#10 + amountstring + #13#10 + misprobstring + #13#10 + alphabetstring;

  FilePath := MainPath + 'python\1_read_generator\';
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
  TCheckExternal_Call := FMain.BGBCreateReadsCreated;
  TCheckExternals.Enabled := true;
end;

procedure TFMain.BGBCreateReadsCreated;
begin
  MCreateReadsres.Lines.LoadFromFile(MainPath + 'python\1_read_generator\out.txt');
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
      PythonPath := 'C:\Python33\';

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

  i := Length(PythonPath);
  while (i > 0) and (PythonPath[i] = '\') do
    Dec(i);

  PythonPath := copy(PythonPath, 1, i) + '\';

  SaveDAB;
end;

procedure TFMain.BCreateDNACreateBasedOnRefClick(Sender: TObject);
var reference: string;
begin
  reference := StringReplace(MReconstructDNAReference.Text, #13#10, '', [rfReplaceAll]);
  if Length(reference) = 0 then
    begin
      if Sender = nil then
        ShowMessage('No reference string can be created!')
      else
        begin
          BReconstructDNACreateReferenceClick(nil);
          BCreateDNACreateBasedOnRefClick(nil);
        end;
      exit;
    end;

  
end;

procedure TFMain.BReconstructDNACreateReferenceClick(Sender: TObject);
begin
  // dostuff
end;

end.

