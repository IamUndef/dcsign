unit uIMultiViewer;

interface

type

  IMultiViewer = interface( IInterface )
    ['{A0E38A31-7B63-421A-B651-7A9325D0EAF6}']

    procedure Show( const Caption : String );
    procedure Hide( const Msg : String );
    procedure AddFile( IsOk : Boolean; const FileName : String;
      const Result : String );

  end;

implementation

end.
