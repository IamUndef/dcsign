unit uIFileModel;

interface

uses SysUtils, uISignContext;

type

  IFileModel = interface( IInterface )
    ['{32EB925C-F9C4-44E9-ABC0-F56E2310D01B}']

    procedure Open( const Directory : String );
    function Read( const FileName : String ) : TBytes;
    function ReadSign( const FileName : String ) : ISignContext;
    procedure DeleteSign( const FileName : String );

  end;

implementation

end.
