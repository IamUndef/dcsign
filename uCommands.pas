unit uCommands;

interface

uses uSimpleCommand, uICommands;

type

  TSignCommand = class( TSimpleCommand, ISignCommand )
  end;

  TSettingCommand = class( TInterfacedObject, ICommand, ISettingCommand )
  end;

implementation

end.
