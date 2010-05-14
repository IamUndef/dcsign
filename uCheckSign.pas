unit uCheckSign;

interface

uses SysUtils, JwaWinCrypt, uIFileModel, uICheckSign;

type

  TCheckSign = class( TInterfacedObject, ICheckSign )
    private
      Model : IFileModel;
      CertSubject_ : String;
      DateTime_ : TDateTime;

      function InitCSP( const CSPName : String ) : HCRYPTPROV;
      function GetCertificate( const Buffer : TBytes ) : PCCERT_CONTEXT;
      function Check( hProv : HCRYPTPROV; pCert : PCCERT_CONTEXT;
        AlgId : Integer; const Buffer : TBytes; const Sign : TBytes ) : Boolean;
      function GetCertSubject( pCert : PCCERT_CONTEXT ) : String; overload;
      function MergeBuffer( LeftBuf : Pointer; LeftBufSize : Integer;
      RightBuf : Pointer; RightBufSize : Integer ) : TBytes;

    protected
      function GetCertSubject() : String; overload;
      function GetDateTime() : TDateTime;

    public
      constructor Create( const FileModel : IFileModel );

      function SingleCheck( const FileName : String ) : Boolean;
      //MultiCheckSign

      procedure ViewCertificate( const FileName : String );

      property CertSubject : String read GetCertSubject;
      property DateTime : TDateTime read GetDateTime;

  end;

implementation

uses Windows, Dialogs, JwaCryptUIApi, JwaWinError, uISignContext;

constructor TCheckSign.Create( const FileModel : IFileModel );
begin
  inherited Create();
  Model := FileModel;
  CertSubject_ := '';
  DateTime_ := 0; 
end;

function TCheckSign.SingleCheck( const FileName: String ) : Boolean;
var
  hProv : HCRYPTPROV;
  Buffer : TBytes;
  SignContext : ISignContext;
  pCert : PCCERT_CONTEXT;
  DateTime : TDateTime;
begin
  Result := false;
  CertSubject_ := '';
  DateTime_ := 0;
  hProv := 0;
  pCert := NIL;
  try
    try
      Buffer := Model.Read( FileName );
      SignContext := Model.ReadSign( FileName );
      Buffer := MergeBuffer( Pointer( @FileName[1] ), Length( FileName ),
        Pointer( @Buffer[0] ),
        Length( Buffer ) );
      DateTime := SignContext.DateTime;
      Buffer := MergeBuffer( Pointer( @Buffer[0] ), Length( Buffer ),
        Pointer( @DateTime ), SizeOf( TDateTime ) );
      hProv := InitCSP( SignContext.CSPName );
      pCert := GetCertificate( SignContext.Certificates[0] );
      Result := Check( hProv, pCert, SignContext.AlgId, Buffer,
        SignContext.Sign );
      if Result then
      begin
        CertSubject_ := GetCertSubject( pCert );
        DateTime_ := SignContext.DateTime;
      end;
    finally
      if Assigned( pCert ) then
        CertFreeCertificateContext( pCert );
      if ( hProv <> 0 ) then
        CryptReleaseContext( hProv, 0 );
    end;
  except
    on E : Exception do
      MessageDlg( E.Message,  mtError, [mbOK], 0 );
  end;
end;

procedure TCheckSign.ViewCertificate( const FileName : String );
var
  i : Integer;
  SignContext : ISignContext;
  hRootStore : HCERTSTORE;
  hMemStore : HCERTSTORE;
  pRootCert : PCCERT_CONTEXT;
  pCert : PCCERT_CONTEXT;
begin
  hRootStore := NIL;
  pRootCert := NIL;
  pCert := NIL;
  try
    try
      SignContext := Model.ReadSign( FileName );
      hRootStore := CertOpenSystemStore ( 0, 'ROOT' );
      if not Assigned( hRootStore ) then
        raise Exception.Create(
          'Не удалось получить доступ к хранилищу доверенных корневых центров сертификации!' );
      if not CertAddEncodedCertificateToStore( hRootStore,
          X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
          @SignContext.Certificates[High( SignContext.Certificates )][0],
          Length( SignContext.Certificates[High( SignContext.Certificates )] ),
          CERT_STORE_ADD_USE_EXISTING,
          @pRootCert ) then
        raise Exception.Create(
          'Не удалось добавить корневой сертификат в хранилище доверенных корневых центров сертификации!' );
      if ( Length( SignContext.Certificates ) > 1 ) then
      begin
        hMemStore := CertOpenStore( CERT_STORE_PROV_MEMORY, 0, 0, 0, NIL );
        if not Assigned( hMemStore ) then
          raise Exception.Create( 'Не удалось создать временное хранилище!' );
        try
          if not CertAddEncodedCertificateToStore( hMemStore,
              X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
              @SignContext.Certificates[0][0],
              Length( SignContext.Certificates[0] ),
              CERT_STORE_ADD_ALWAYS,
              @pCert ) then
            raise Exception.Create(
              'Не удалось добавить сертификат во временное хранилище!' );
          for i := 1 to High( SignContext.Certificates ) - 1 do
            if not CertAddEncodedCertificateToStore( hMemStore,
                X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
                @SignContext.Certificates[i][0],
                Length( SignContext.Certificates[i] ),
                CERT_STORE_ADD_ALWAYS,
                NIL ) then
              raise Exception.Create(
                'Не удалось добавить сертификат во временное хранилище!' );
          CryptUIDlgViewContext( CERT_STORE_CERTIFICATE_CONTEXT, pCert, 0, NIL,
            0, NIL );
        finally
          if Assigned( pCert ) then
            CertFreeCertificateContext( pCert );
          CertCloseStore( hMemStore, {CERT_CLOSE_STORE_FORCE_FLAG} 0 );
        end;
      end else
        CryptUIDlgViewContext( CERT_STORE_CERTIFICATE_CONTEXT, pRootCert, 0,
          NIL, 0, NIL );
    finally
      if Assigned( pRootCert ) then
        CertFreeCertificateContext( pRootCert );
      if Assigned( hRootStore ) then
        CertCloseStore( hRootStore, {CERT_CLOSE_STORE_FORCE_FLAG} 0 );
    end;
  except
    on E : Exception do
      MessageDlg( E.Message,  mtError, [mbOK], 0 );
  end;
end;

function TCheckSign.GetCertSubject() : String;
begin
  Result := CertSubject_;
end;

function TCheckSign.GetDateTime() : TDateTime;
begin
  Result := DateTime_;
end;

function TCheckSign.InitCSP( const CSPName : String ) : HCRYPTPROV;
var
  Index : Integer;
  ProvType : DWORD;
  Size : DWORD;
  Data : String;
begin
  Result := 0;
  Index := 0;
  while CryptEnumProviders( Index, NIL, 0, ProvType, NIL, Size ) do
  begin
    SetLength( Data, Size );
		if ( CryptEnumProviders( Index, NIL, 0, ProvType, @Data[1], Size ) and
      ( ( CSPName + #0 ) = Data ) ) then
      break;
    Inc( Index );
  end;
  CryptAcquireContext( Result, '', PChar( CSPName ), ProvType,
    CRYPT_VERIFYCONTEXT );
  if ( Result = 0 ) then
    raise Exception.Create( 'Не удалось инициализировать криптопровайдер - "' +
      CSPName + '"!' );
end;

function TCheckSign.GetCertificate( const Buffer : TBytes ) : PCCERT_CONTEXT;
begin
  Result := NIL;
  if Assigned( Buffer ) then
  begin
    Result := CertCreateCertificateContext(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      @Buffer[0], Length( Buffer ) );
    if not Assigned( Result ) then
      raise Exception.Create( 'Не удалось получить сертификат!' );
  end;
end;

function TCheckSign.Check( hProv : HCRYPTPROV; pCert : PCCERT_CONTEXT;
  AlgId : Integer; const Buffer : TBytes; const Sign : TBytes ) : Boolean;
var
  IsError : Boolean;
  hKey : HCRYPTKEY;
  hHash : HCRYPTHASH;
begin
  Result := false;
  IsError := true;
  hKey := 0;
  hHash := 0;
  try
    if ( CryptImportPublicKeyInfo( hProv,
        PKCS_7_ASN_ENCODING or X509_ASN_ENCODING,
        @pCert.pCertInfo.SubjectPublicKeyInfo, hKey ) and
        CryptCreateHash( hProv, AlgId, 0, 0, hHash ) and
        CryptHashData( hHash, @Buffer[0], Length( Buffer ), 0 ) ) then
    begin
      Result := CryptVerifySignature( hHash, @Sign[0],
        Length( Sign ), hKey, NIL, 0 );
      IsError := false;
    end;
  finally
    if ( hHash <> 0 ) then
      CryptDestroyHash( hHash );
    if ( hKey <> 0 ) then
      CryptDestroyKey( hKey );
  end;
  if IsError then
    raise Exception.Create( 'Не удалось проверить подпись данных!' );
end;

function TCheckSign.GetCertSubject( pCert : PCCERT_CONTEXT )
  : String;
var
  Size : Integer;
begin
  Result := '';
  Size := CertGetNameString( pCert, CERT_NAME_SIMPLE_DISPLAY_TYPE,
    0, NIL, NIL, 0 );
  if ( Size > 1 ) then
  begin
    SetLength( Result, Size );
    if ( CertGetNameString( pCert, CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, NIL,
        @Result[1], Size ) = 0 ) then
      Result := '';
  end;
end;

function TCheckSign.MergeBuffer( LeftBuf : Pointer; LeftBufSize : Integer;
  RightBuf : Pointer; RightBufSize : Integer ) : TBytes;
begin
  Result := NIL;
  try
    SetLength( Result, LeftBufSize + RightBufSize );
    CopyMemory( Pointer( @Result[0] ), LeftBuf, LeftBufSize );
    CopyMemory( Pointer( @Result[LeftBufSize] ), RightBuf, RightBufSize );
  except
    on E : Exception do
      MessageDlg( E.Message,  mtError, [mbOK], 0 );  
  end;
end;

end.
