library dcsetsign;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  FastMM4 in '..\fastmm\FastMM4.pas',
  FastMM4Messages in '..\fastmm\FastMM4Messages.pas',
  SysUtils,
  Classes,
  uSetSign in 'uSetSign.pas',
  uICommands in '..\common\uICommands.pas',
  uIModule in '..\common\uIModule.pas',
  JwaCryptUIApi in '..\common\JwaCryptUIApi.pas',
  JwaNtStatus in '..\common\JwaNtStatus.pas',
  JwaWinBase in '..\common\JwaWinBase.pas',
  JwaWinCrypt in '..\common\JwaWinCrypt.pas',
  JwaWinError in '..\common\JwaWinError.pas',
  JwaWinNT in '..\common\JwaWinNT.pas',
  JwaWinType in '..\common\JwaWinType.pas',
  JwaWinDLLNames in '..\common\JwaWinDLLNames.pas',
  uSignContext in '..\common\uSignContext.pas',
  uCommands in 'uCommands.pas',
  uSimpleCommand in '..\common\uSimpleCommand.pas',
  uISignContext in '..\common\uISignContext.pas';

exports
  GetInstance;

{$R *.res}

begin
end.
