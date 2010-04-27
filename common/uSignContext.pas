unit uSignContext;

interface

uses SysUtils, uISignContext;

type

  TSignContext = class( TInterfacedObject, ISignContext )
    private
      CSPName_ : String;
      AlgId_ : Integer;
      DateTime_ : TDateTime;
      Sign_ : TBytes;
      Certificates_ : TBBytes;

      function IsValid() : Boolean;

    protected
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

    public
      constructor Create(); overload;
      constructor Create( const Buffer : TBytes ); overload;

      property Buffer : TBytes read Pack write UnPack;
      property CSPName : String read GetCSPName write SetCSPName;
      property AlgId : Integer read GetAlgId write SetAlgId;
      property DateTime : TDateTime read GetDateTime write SetDateTime;
      property Sign : TBytes read GetSign write SetSign;
      property Certificates : TBBytes read GetCertificates
        write SetCertificates;

  end;

implementation

uses Windows;

constructor TSignContext.Create();
begin
  CSPName_ := '';
  AlgId_ := 0;
  DateTime_ := 0;
  Sign_ := NIL;
  Certificates_ := NIL;
end;

constructor TSignContext.Create( const Buffer: TBytes );
begin
  UnPack( Buffer );
end;

function TSignContext.IsValid() : Boolean;
begin
  if ( ( CSPName_ <> '' ) and
      ( AlgId_ <> 0 ) and
      ( DateTime_ <> 0 ) and
      ( Sign_ <> NIL ) and
      ( Certificates_ <> NIL ) ) then
    Result := true
  else
    Result := false;
end;

procedure TSignContext.UnPack( const Buffer : TBytes );
begin
// ������ Buffer
end;

function TSignContext.Pack() : TBytes;
var
  i : Integer;
  BufSize : Integer;
  BufPos : PChar;
begin
  Result := NIL;
  if IsValid() then
  begin
    BufSize := Length( CSPName_ ) + SizeOf( Char ) +
      ( Length( Certificates_ ) + 3 )*SizeOf( Integer ) + SizeOf( TDateTime ) +
      Length( Sign_ );
    for i := 0 to Length( Certificates_ ) - 1 do
      Inc( BufSize, Length( Certificates_[i] ) );
    SetLength( Result, BufSize );
    CopyMemory( Pointer( @Result[0] ), Pointer( @CSPName_[1] ),
      Length( CSPName_ ) );
    BufPos := PChar( @Result[0] ) + Length( CSPName_ );
    BufPos^ := #0;
    Inc( BufPos );
    PInteger( BufPos )^ := AlgId_;
    Inc( BufPos, SizeOf( Integer ) );
    PDateTime( BufPos )^ := DateTime;
    Inc( BufPos, SizeOf( TDateTime ) );
    PInteger( BufPos )^ := Length( Sign_ );
    Inc( BufPos, SizeOf( Integer ) );
    CopyMemory( Pointer( BufPos ), Pointer( @Sign_[0] ),
      Length( Sign_ ) );
    Inc( BufPos, Length( Sign_ ) );
    PInteger( BufPos )^ := Length( Certificates_ );
    Inc( BufPos, SizeOf( Integer ) );
    for i := 0 to Length( Certificates_ ) - 1 do
    begin
      PInteger( BufPos )^ := Length( Certificates_[i] );
      Inc( BufPos, SizeOf( Integer ) );
      CopyMemory( Pointer( BufPos ), Pointer( @Certificates_[i][0] ),
        Length( Certificates_[i] ) );
      Inc( BufPos, Length( Certificates_[i] ) );
    end;
  end;
end;

function TSignContext.GetCSPName() : String;
begin
  Result := CSPName_;
end;

procedure TSignContext.SetCSPName( const CSPName : String );
begin
  CSPName_ := CSPName;
end;

function TSignContext.GetAlgId() : Integer;
begin
  Result := AlgId_;
end;

procedure TSignContext.SetAlgId( const AlgId : Integer );
begin
  AlgId_ := AlgId;
end;

function TSignContext.GetDateTime() : TDateTime;
begin
  Result := DateTime_;
end;

procedure TSignContext.SetDateTime( const DateTime : TDateTime );
begin
  DateTime_ := DateTime;
end;

function TSignContext.GetSign() : TBytes;
begin
  Result := Sign_;
end;

procedure TSignContext.SetSign( const Sign : TBytes );
begin
  Sign_ := Sign;
end;

function TSignContext.GetCertificates() : TBBytes;
begin
  Result := Certificates_;
end;

procedure TSignContext.SetCertificates( const Certificates : TBBytes );
begin
  Certificates_ := Certificates;
end;

end.
