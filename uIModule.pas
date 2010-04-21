unit uIModule;

interface

uses uICommands;

type

  IModule = interface( IInterface )
    ['{C52A6A39-4FB3-4BAC-933D-2EDA8C70E557}']

    procedure Execute( Command : ICommand );

  end;

implementation

end.
