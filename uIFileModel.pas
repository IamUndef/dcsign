unit uIFileModel;

interface

uses Classes, SysUtils, uIMultiViewer, uISignContext;

type

  IFileModel = interface( IInterface )
    ['{32EB925C-F9C4-44E9-ABC0-F56E2310D01B}']

    procedure Open( const Directory : String; Files : TStrings );
    function Read( const FileName : String ) : TBytes;
    function ReadSign( const FileName : String ) : ISignContext;
    procedure SingleDeleteSign( const FileName : String );
    procedure MultiDeleteSign( const Viewer : IMultiViewer; Files : TStrings );
    procedure SingleCopy( const Dir : String; const FileName : String );
    procedure MultiCopy( const Viewer : IMultiViewer; const Dir : String;
      Files : TStrings );
    function GetDirectory() : String;

    property Directory : String read GetDirectory;

  end;

implementation

end.
