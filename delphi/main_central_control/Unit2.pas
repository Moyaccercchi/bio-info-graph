unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFOptions = class(TForm)
    Label1: TLabel;
    EPythonPath: TEdit;
    PHUDBtm: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

end.
