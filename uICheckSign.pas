unit uICheckSign;

interface

uses Classes, uIMultiViewer;

type

  ICheckSign = interface( IInterface )

    function SingleCheck( const FileName : String ) : Boolean;
    procedure MultiCheckSign( const Viewer : IMultiViewer; Files : TStrings );
    procedure ViewCertificate( const FileName : String );
    function GetCertSubject() : String;
    function GetDateTime() : TDateTime;

    property CertSubject : String read GetCertSubject;
    property DateTime : TDateTime read GetDateTime;

  end;

implementation

end.
