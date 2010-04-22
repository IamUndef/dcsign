unit uICommands;

interface

uses SysUtils;

type

  ICommand = interface( IInterface )
    ['{44159665-1DCC-438D-8059-ED8EA035C467}']
  end;

  ISimpleCommand = interface( ICommand )
    ['{681CA845-B4D0-48D9-9CA8-D03F6BF16A78}']

    function GetArgument() : String;

    property Argument : String read GetArgument;

  end;

  ISignCommand = interface( ISimpleCommand )
    ['{0C2DA0D3-56FD-4AA5-A13C-E77C56FB4A7B}']
  end;

  IReadCommand = interface( ISimpleCommand )
    ['{BC4A3525-545F-4E43-826D-89042D07DB02}']

    procedure SetData( Data : TBytes );
    function GetData() : TBytes;

    property Data : TBytes read GetData write SetData;

  end;

implementation

end.
