program dcsign;

uses
  FastMM4 in 'fastmm\FastMM4.pas',
  FastMM4Messages in 'fastmm\FastMM4Messages.pas',
  Forms,
  uMainModule in 'uMainModule.pas' {MainModule},
  uFileModel in 'uFileModel.pas',
  uIFileModel in 'uIFileModel.pas',
  uICommands in 'common\uICommands.pas',
  uIModule in 'common\uIModule.pas',
  uSignContext in 'common\uSignContext.pas',
  uCommands in 'uCommands.pas',
  uSimpleCommand in 'common\uSimpleCommand.pas',
  uISignContext in 'common\uISignContext.pas',
  uICheckSign in 'uICheckSign.pas',
  uCheckSign in 'uCheckSign.pas',
  JwaCryptUIApi in 'common\JwaCryptUIApi.pas',
  JwaNtStatus in 'common\JwaNtStatus.pas',
  JwaWinBase in 'common\JwaWinBase.pas',
  JwaWinCrypt in 'common\JwaWinCrypt.pas',
  JwaWinDLLNames in 'common\JwaWinDLLNames.pas',
  JwaWinError in 'common\JwaWinError.pas',
  JwaWinNT in 'common\JwaWinNT.pas',
  JwaWinType in 'common\JwaWinType.pas',
  uCommand in 'common\uCommand.pas',
  uMultiViewerForm in 'uMultiViewerForm.pas' {MultiViewerForm},
  uIMultiViewer in 'common\uIMultiViewer.pas',
  uMultiViewer in 'uMultiViewer.pas',
  uFileIconList in 'uFileIconList.pas',
  uAbout in 'uAbout.pas' {About};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Подпись документов - ГУЗ «ЗабКДЦ»';
  Application.CreateForm(TMainModule, MainModule);
  Application.CreateForm(TAbout, About);
  Application.Run;
end.
