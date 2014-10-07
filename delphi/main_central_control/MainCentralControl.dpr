program MainCentralControl;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Main Central Control';
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
