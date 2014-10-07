program MainCentralControl;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FMain},
  Unit2 in 'Unit2.pas' {FOptions};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Main Central Control';
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TFOptions, FOptions);
  Application.Run;
end.
