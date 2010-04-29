unit uISignContext;

interface

uses SysUtils;

type

  TBBytes = array of TBytes;

  ISignContext = interface( IInterface )
    ['{9B6C58C2-BD98-403B-9B7D-2A4D4EC0B918}']

    function IsValid() : Boolean;
    procedure UnPack( const Buffer : TBytes );
    function Pack() : TBytes;
    function GetCSPName() : String;
    procedure SetCSPName( const CSPName : String );
    function GetAlgId() : Integer;
    procedure SetAlgId( const AlgId : Integer );
    function GetDateTime() : TDateTime;
    procedure SetDateTime( const DateTime : TDateTime );
    function GetSign() : TBytes;
    procedure SetSign( const Sign : TBytes );
    function GetCertificates() : TBBytes;
    procedure SetCertificates( const Certificates : TBBytes );

    property Buffer : TBytes read Pack write UnPack;
    property CSPName : String read GetCSPName write SetCSPName;
    property AlgId : Integer read GetAlgId write SetAlgId;
    property DateTime : TDateTime read GetDateTime write SetDateTime;
    property Sign : TBytes read GetSign write SetSign;
    property Certificates : TBBytes read GetCertificates
      write SetCertificates;
      
  end;

implementation

end.
