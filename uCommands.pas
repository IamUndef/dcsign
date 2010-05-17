unit uCommands;

interface

uses uICommands, uCommand, uSimpleCommand;

type

  TSignCommand = class( TSimpleCommand, ISignCommand )
  end;

  TSettingCommand = class( TCommand, ISettingCommand )
  end;

implementation

end.
