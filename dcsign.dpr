program dcsign;

uses
  Forms,
  uMainModule in 'uMainModule.pas' {MainModule},
  uIModule in 'uIModule.pas',
  uCommands in 'uCommands.pas',
  uFileModel in 'uFileModel.pas',
  uSignContext in 'uSignContext.pas',
  uIFileModel in 'uIFileModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Подпись документов - ГУЗ «ЗабКДЦ»';
  Application.CreateForm(TMainModule, MainModule);
  Application.Run;
end.
