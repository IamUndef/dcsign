unit uICommands;

interface

uses SysUtils, uISignContext;

type

  ICommand = interface( IInterface )
    ['{44159665-1DCC-438D-8059-ED8EA035C467}']

    function GetExceptionMsg() : String;
    procedure SetExceptionMsg( Msg : String );

    property ExceptionMsg : String read GetExceptionMsg write SetExceptionMsg;

  end;

  ISimpleCommand = interface( ICommand )
    ['{681CA845-B4D0-48D9-9CA8-D03F6BF16A78}']

    function GetFileName() : String;

    property FileName : String read GetFileName;

  end;

  ISignCommand = interface( ISimpleCommand )
    ['{0C2DA0D3-56FD-4AA5-A13C-E77C56FB4A7B}']
  end;

  IReadCommand = interface( ISimpleCommand )
    ['{BC4A3525-545F-4E43-826D-89042D07DB02}']

    function GetBuffer() : TBytes;
    procedure SetBuffer( const Buffer : TBytes );

    property Buffer : TBytes read GetBuffer write SetBuffer;

  end;

  ICreateSignCommand = interface( ISimpleCommand )
    ['{B5C14C11-B32A-4CDE-89C3-C1C427B2E645}']

    function GetSignContext() : ISignContext;

    property SignContext : ISignContext read GetSignContext;

  end;

  ISettingCommand = interface( ICommand )
    ['{8F194E24-6925-4810-BC18-866159C3D335}']
  end;


implementation

end.
