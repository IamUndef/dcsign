program dcsign;

uses
  Forms,
  uMainModule in 'uMainModule.pas' {MainModule},
  uFileModel in 'uFileModel.pas',
  uIFileModel in 'uIFileModel.pas',
  uICommands in 'common\uICommands.pas',
  uIModule in 'common\uIModule.pas',
  uSignContext in 'common\uSignContext.pas',
  uCommands in 'uCommands.pas',
  uSimpleCommand in 'common\uSimpleCommand.pas',
  uISignContext in 'common\uISignContext.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Подпись документов - ГУЗ «ЗабКДЦ»';
  Application.CreateForm(TMainModule, MainModule);
  Application.Run;
end.
