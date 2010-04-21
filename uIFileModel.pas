unit uIFileModel;

interface

uses SysUtils, uSignContext;

type

  IFileModel = interface( IInterface )
    ['{32EB925C-F9C4-44E9-ABC0-F56E2310D01B}']

    procedure Open( Directory : String );
    function Read( FileName : String ) : TBytes;
    function ReadSign( FileName : String ) : TSignContext;
    procedure DeleteSign( FileName : String );

  end;

implementation

end.
