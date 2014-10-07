unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellApi, ExtCtrls;

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
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure BCreateDNACopyClick(Sender: TObject);
    procedure BCreateDNACreateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BGBCreateReadsCreateClick(Sender: TObject);
    procedure TCheckExternalsTimer(Sender: TObject);
  private
    function copyend(str, substr: string; depth: Integer): string;
    procedure BGBCreateReadsCreated;
  public
    { Public declarations }
  end;

var
  FMain: TFMain;
  Path, MainPath, PythonPath, TCheckExternal_FilePath: string;
  TCheckExternal_Call: procedure of object;

implementation

{$R *.dfm}

procedure TFMain.FormResize(Sender: TObject);
begin
  GBCreateDNA.Width := FMain.ClientWidth - (2 * GBCreateDNA.Left);
  GBCreateReads.Width := GBCreateDNA.Width;
  GBReconstructDNA.Width := GBCreateDNA.Width;

  MCreateDNAres.Width := GBCreateDNA.ClientWidth - (2 * MCreateDNAres.Left);
  BCreateDNACreate.Width := (MCreateDNAres.Width - MCreateDNAres.Left) div 2;
  BCreateDNACopy.Width := BCreateDNACreate.Width;
  BCreateDNACopy.Left := GBCreateDNA.ClientWidth - (BCreateDNACreate.Left + BCreateDNACopy.Width);
  LCreateDNAAlphabet.Left := BCreateDNACopy.Left;
  ECreateDNAAlphabet.Left := LCreateDNAAlphabet.Left + LCreateDNAAlphabet.Width + 4;
  ECreateDNAAlphabet.Width := GBCreateDNA.ClientWidth - (BCreateDNACreate.Left + ECreateDNAAlphabet.Left);
  LCreateDNALengthBasepairs.Left := BCreateDNACreate.Width + BCreateDNACreate.Left - (LCreateDNALengthBasepairs.Width);
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
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  FormResize(nil);
end;

procedure TFMain.BCloseClick(Sender: TObject);
begin
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
  PythonPath := 'D:\Dropbox\system\Python33\';
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
    dnastring, lengthstring, amountstring, misprobstring, FilePath, InFilePath, OutFilePath: string;
begin
  MCreateReadsres.Text := '';

  output := TStringList.Create;

  dnastring := StringReplace(MCreateDNAres.Text, #13#10, '', [rfReplaceAll]);
  if Length(dnastring) = 0 then
    begin
      if (Sender = nil) then
        begin
          ShowMessage('No DNA string can be generated.');
          exit;
        end;
      BCreateDNACreateClick(nil);
      BGBCreateReadsCreateClick(nil);
      exit;
    end;
  lengthstring := ECreateReadsLength.Text;
  amountstring := ECreateReadsAmount.Text;
  misprobstring := ECreateReadsMisProb.Text;

  output.Text := dnastring + #13#10 + lengthstring + #13#10 + amountstring + #13#10 + misprobstring;

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

end.

