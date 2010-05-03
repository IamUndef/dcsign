unit uICheckSign;

interface

type

  ICheckSign = interface( IInterface )

    function SingleCheck( const FileName : String ) : Boolean;
    procedure ViewCertificate( const FileName : String );
    function GetCertSubject() : String;
    function GetDateTime() : TDateTime;

    property CertSubject : String read GetCertSubject;
    property DateTime : TDateTime read GetDateTime;

  end;

implementation

end.
