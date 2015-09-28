unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFOptions = class(TForm)
    PHUDBtm: TPanel;
    Button1: TButton;
    Button2: TButton;
    GBEnvironment: TGroupBox;
    Label1: TLabel;
    EPythonPath: TEdit;
    GBReset: TGroupBox;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    GroupBox1: TGroupBox;
    CBpauseBatchfiles: TCheckBox;
    Button6: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FOptions: TFOptions;

implementation

uses Unit1;

{$R *.dfm}

procedure TFOptions.Button2Click(Sender: TObject);
begin
  FMain.SaveOptions;
  Close;
end;

procedure TFOptions.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFOptions.Button3Click(Sender: TObject);
begin
  FMain.ResetTo(0);
  Close;
end;

procedure TFOptions.Button4Click(Sender: TObject);
begin
  FMain.ResetTo(1);
  Close;
end;

procedure TFOptions.Button5Click(Sender: TObject);
begin
  FMain.ResetTo(2);
  Close;
end;

procedure TFOptions.Button6Click(Sender: TObject);
begin
  FMain.ResetTo(3);
  Close;
end;

end.
