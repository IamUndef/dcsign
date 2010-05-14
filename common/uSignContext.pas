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

      function IsValid() : Boolean;

      property Buffer : TBytes read Pack write UnPack;
      property CSPName : String read GetCSPName write SetCSPName;
      property AlgId : Integer read GetAlgId write SetAlgId;
      property DateTime : TDateTime read GetDateTime write SetDateTime;
      property Sign : TBytes read GetSign write SetSign;
      property Certificates : TBBytes read GetCertificates
        write SetCertificates;

  end;

implementation

uses Windows, Classes;

constructor TSignContext.Create();
begin
  inherited;
  CSPName_ := '';
  AlgId_ := 0;
  DateTime_ := 0;
  Sign_ := NIL;
  Certificates_ := NIL;
end;

constructor TSignContext.Create( const Buffer: TBytes );
begin
  inherited Create();
  UnPack( Buffer );
end;

function TSignContext.IsValid() : Boolean;
begin
  Result := ( ( CSPName_ <> '' ) and
    ( AlgId_ <> 0 ) and
    ( DateTime_ <> 0 ) and
    ( Sign_ <> NIL ) and
    ( Certificates_ <> NIL ) );
end;

procedure TSignContext.UnPack( const Buffer : TBytes );
var
  i : Integer;
  BufPos : PChar;
  SignSize : Integer;
  CertsCount : Integer;
  CertSize : Integer;

function EndOfBuffer( AddSize : Integer ) : Boolean;
begin
  if ( AddSize >= 0 ) then
    Result := ( Integer( BufPos - PChar( Buffer ) ) + AddSize ) > Length( Buffer )
  else
    Result := true;
end;

begin
  if Assigned( Buffer ) then
  begin
    CSPName_ := '';
    AlgId_ := 0;
    DateTime_ := 0;
    Sign := NIL;
    Certificates_ := NIL;
    BufPos := PChar( Buffer );
    while ( ( BufPos <> PChar( @Buffer[Length( Buffer )] ) ) and ( BufPos^ <> #0 ) ) do
      Inc( BufPos );
    if ( BufPos <> PChar( @Buffer[Length( Buffer )] ) ) then
    begin
      CSPName_ := PChar( Buffer );
      if not EndOfBuffer( SizeOf( Char ) + 2*SizeOf( Integer ) +
          SizeOf( TDateTime ) ) then 
      begin
        // Получаем AlgId, DateTime и размер Sign
        Inc( BufPos, SizeOf( Char ) );
        AlgId := PInteger( BufPos )^;
        Inc( BufPos, SizeOf( Integer ) );
        DateTime_ := PDateTime( BufPos )^;
        Inc( BufPos, SizeOf( TDateTime ) );
        SignSize := PInteger( BufPos )^;
        Inc( BufPos, SizeOf( Integer ) );
        if not EndOfBuffer( SignSize ) then
        begin
          // Получаем Sign
          SetLength( Sign_, SignSize );
          CopyMemory( Pointer( @Sign[0] ), Pointer( BufPos ), SignSize );
          Inc( BufPos, SignSize );
          if not EndOfBuffer( SizeOf( Integer ) ) then
          begin
            // Получаем количество сертификатов
            CertsCount := PInteger( BufPos )^;
            Inc( BufPos, SizeOf( Integer ) );
            if not EndOfBuffer( CertsCount*SizeOf( Integer ) ) then
            begin
              SetLength( Certificates_, CertsCount );
              // Получаем сертификаты
              for i := 0 to CertsCount - 1 do
              begin
                if not EndOfBuffer( SizeOf( Integer ) ) then
                begin
                  // Получаем размер сертификата
                  CertSize := PInteger( BufPos )^;
                  Inc( BufPos, SizeOf( Integer ) );
                  if not EndOfBuffer( CertSize ) then
                  begin
                    // Получаем данные сертификата
                    SetLength( Certificates_[i], CertSize );
                    CopyMemory( Pointer( @Certificates_[i][0] ),
                      Pointer( BufPos ),
                      CertSize );
                    Inc( BufPos, Length( Certificates_[i] ) );
                  end else
                    Certificates_ := NIL;
                end else
                  Certificates_ := NIL;
                if not Assigned( Certificates_ ) then
                  Break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
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
