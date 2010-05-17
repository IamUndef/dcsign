unit uIMultiViewer;

interface

type

  IMultiViewer = interface( IInterface )
    ['{A0E38A31-7B63-421A-B651-7A9325D0EAF6}']

    procedure Show( Caption : String );
    procedure Hide( Msg : String );
    procedure AddFile( IsOk : Boolean; FileName : String; Result : String );

  end;

implementation

end.
