unit uSetSign;

interface

uses SysUtils, Windows, JwaWinCrypt, uIModule, uICommands, uISignContext;

type
  TSetSign = class( TInterfacedObject, IModule )
    private
      const
        DEFAULT_CSP =
          'Crypto-Pro GOST R 34.10-2001 Cryptographic Service Provider';
        DEFAULT_ALGID = 32798;
        DEFAULT_CONTAINER = '';
        
    private
      type
        TKeyInfo = record
          KeyType : DWORD;
          pCert : PCCERT_CONTEXT;
        end;

        TSignInfo = record
          DateTime : TDateTime;
          Sign : TBytes;
        end;
        
    private
      Model : IModule;
      CSPName : String;
      AlgId : Integer;
      Container : String;
      
      procedure Execute( const Command : ISignCommand ); overload;
      procedure Execute( const Command : ISettingCommand ); overload;

      function InitCSP() : HCRYPTPROV;
      function GetCertificate( hProv : HCRYPTPROV ) : TKeyInfo;
      function GetChainCerts( pCert : PCCERT_CONTEXT ) : TBBytes;
      function Sign( hProv : HCRYPTPROV; KeyType : DWORD;
        const Buffer : TBytes ) : TSignInfo;
      function MergeBuffer( LeftBuf : Pointer; LeftBufSize : Integer;
        RightBuf : Pointer; RightBufSize : Integer ) : TBytes;

      function SingleSign( const Command : IReadCommand ) : Boolean;
      // MultiSign
      
    public
      constructor Create( const FileModel : IModule );

      function Execute( const Command : ICommand ) : Boolean; overload;

  end;

  function GetInstance( const FileModel : IModule ) : IModule; export;

implementation

uses Controls, Dialogs, uCommands, uSetting, uSelectContainer;

function GetInstance( const FileModel : IModule ) : IModule; export;
begin
  Result := TSetSign.Create( FileModel );
end;

constructor TSetSign.Create( const FileModel: IModule );
begin
  Model := FileModel;
  CSPName := DEFAULT_CSP;
  AlgId := DEFAULT_ALGID;
  Container := DEFAULT_CONTAINER;
end;

function TSetSign.Execute( const Command : ICommand ) : Boolean;
begin
  Result := false;
  try
    if Supports( Command, ISignCommand ) then
      Execute( Command as ISignCommand )
    else if Supports( Command, ISettingCommand ) then
      Execute( Command as ISettingCommand )
    else
      raise Exception.Create( 'Команда не поддерживается!' );
    Result := true;
  except
    on E : Exception do
      MessageDlg( E.Message,  mtError, [mbOK], 0 );
  end;
end;

procedure TSetSign.Execute( const Command : ISignCommand );
var
  ReadCmd : IReadCommand;
begin
  ReadCmd := TReadCommand.Create( Command.FileName );
  if Model.Execute( ReadCmd as ICommand ) then
    SingleSign( ReadCmd );
end;

procedure TSetSign.Execute( const Command : ISettingCommand );
var
  Setting : TSetting;
begin
  Setting := NIL;
  try
    Setting := TSetting.Create( NIL );
    Setting.CSPName := CSPName;
    Setting.AlgId := AlgId;
    if ( mrOk = Setting.ShowModal() ) then
    begin
      CSPName := Setting.CSPName;
      AlgId := Setting.AlgId;
    end;
  finally
    if Assigned( Setting ) then
      Setting.Free;
  end;
end;

function TSetSign.InitCSP() : HCRYPTPROV;
var
  Index : Integer;
  ProvType : DWORD;
  Size : DWORD;
  Data : String;
  SelectContainer : TSelectContainer;
begin
  Result := 0;
  Index := 0;
  SelectContainer := NIL;
  while CryptEnumProviders( Index, NIL, 0, ProvType, NIL, Size ) do
  begin
    SetLength( Data, Size );
		if ( CryptEnumProviders( Index, NIL, 0, ProvType, @Data[1], Size ) and
      ( ( CSPName + #0 ) = Data ) ) then
      break;
    Inc( Index );
  end;
  try
    while ( ( Container = '' ) or
        not CryptAcquireContext( Result, PChar( Container ), PChar( CSPName ),
          ProvType, 0 ) ) do
    begin
      if not CryptAcquireContext( Result, '', PChar( CSPName ), ProvType,
          CRYPT_VERIFYCONTEXT ) then
        Break
      else
      begin
        try
          if not Assigned( SelectContainer ) then
            SelectContainer := TSelectContainer.Create( @Result );
          SelectContainer.Container := Container;
          SelectContainer.ShowModal();
          if ( mrCancel = SelectContainer.ModalResult ) then
            Break
          else
            Container := SelectContainer.Container;
        finally
          CryptReleaseContext( Result, 0 );
          Result := 0;
        end;
      end;
    end;
  finally
    if Assigned( SelectContainer ) then
      SelectContainer.Free;
  end;
  if ( Result = 0 ) then
    raise Exception.Create( 'Не удалось инициализировать криптопровайдер - "' +
      CSPName + '"!' );
end;

function TSetSign.GetCertificate( hProv : HCRYPTPROV ) : TKeyInfo;
var
  KeySize : DWORD;
  KeyBuf : TBytes;
  hUserKey : HCRYPTKEY;
  CertSize : DWORD;
  CertBuf : TBytes;
  hStore : HCERTSTORE;
begin
  Result.pCert := NIL;
  Result.KeyType := AT_SIGNATURE;
  if not CryptExportPublicKeyInfo( hProv, Result.KeyType,
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, NIL, KeySize ) then
  begin
    Result.KeyType := AT_KEYEXCHANGE;
    if not CryptExportPublicKeyInfo( hProv, Result.KeyType,
        X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, NIL, KeySize ) then
      raise Exception.Create( 'Не удалось получить открытый ключ!' );
  end;
  SetLength( KeyBuf, KeySize );
  if not CryptExportPublicKeyInfo( hProv, Result.KeyType,
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      PCERT_PUBLIC_KEY_INFO( @KeyBuf[0] ), KeySize ) then
    raise Exception.Create( 'Не удалось получить открытый ключ!' );
  hStore := CertOpenSystemStore( hProv, 'MY' );
  if not Assigned( hStore ) then
    raise Exception.Create(
      'Не удалось получить доступ к личному хранилищу сертификатов!' );
  hUserKey := 0;
  try
    Result.pCert := CertFindCertificateInStore( hStore,
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, 0, CERT_FIND_PUBLIC_KEY,
      Pointer( @KeyBuf[0] ), NIL );
    if ( not Assigned( Result.pCert ) and
        CryptGetUserKey( hProv, Result.KeyType, hUserKey ) ) then
    begin
      if ( CryptGetKeyParam( hUserKey, KP_CERTIFICATE, NIL, CertSize, 0 ) and
          ( CertSize <> 0 ) ) then
      begin
        SetLength( CertBuf, CertSize );
        if CryptGetKeyParam( hUserKey, KP_CERTIFICATE, @CertBuf[0],
            CertSize, 0 ) then
          Result.pCert := CertCreateCertificateContext(
            PKCS_7_ASN_ENCODING or X509_ASN_ENCODING,
      	    @CertBuf[0], CertSize );
      end;
      if Assigned( Result.pCert ) then
        CertAddCertificateContextToStore( hStore, Result.pCert,
          CERT_STORE_ADD_REPLACE_EXISTING, NIL );
    end;
  finally
    if ( hUserKey <> 0 ) then
      CryptDestroyKey( hUserKey );  
    CertCloseStore( hStore, 0 );
  end;
  if not Assigned( Result.pCert ) then
    raise Exception.Create( 'Не удалось получить сертификат!' );
end;

function TSetSign.GetChainCerts( pCert : PCCERT_CONTEXT ) : TBBytes;
type
  PPCERT_CHAIN_ELEMENT = ^PCERT_CHAIN_ELEMENT;

var
  i : Integer;
  ChainPara : CERT_CHAIN_PARA;
  pChainContext : PCCERT_CHAIN_CONTEXT;
  ErrorMsg : String;
  pChainElement : PPCERT_CHAIN_ELEMENT;
begin
  Result := NIL;
  ZeroMemory( Pointer( @ChainPara ), SizeOf( CERT_CHAIN_PARA ) );
  ChainPara.cbSize := SizeOf( CERT_CHAIN_PARA );
	ChainPara.RequestedUsage.dwType := USAGE_MATCH_TYPE_AND;
  if not CertGetCertificateChain( 0, pCert, NIL, NIL, @ChainPara,
      CERT_CHAIN_CACHE_END_CERT or CERT_CHAIN_REVOCATION_CHECK_END_CERT,
			NIL, @pChainContext )  then
    raise Exception.Create(
      'Не удалось получить контекст цепочки сертификатов!' );
  try
    ErrorMsg := '';
    if ( pChainContext.TrustStatus.dwErrorStatus <> CERT_TRUST_NO_ERROR ) then
    begin
      pChainElement :=
        PPCERT_CHAIN_ELEMENT( PChar( pChainContext.rgpChain^.rgpElement ) +
        ( pChainContext.rgpChain^.cElement - 1 )*SizeOf( PPCERT_CHAIN_ELEMENT ) );
      for i := 0 to pChainContext.rgpChain^.cElement - 1 do
      begin
        case ( pChainElement^.TrustStatus.dwErrorStatus ) of
          // Для сертификатов и цепочек
          CERT_TRUST_NO_ERROR : {ErrorMsg :=  'Сертификат действителен'};
          CERT_TRUST_IS_NOT_TIME_VALID : ErrorMsg :=
            '"Истек срок действия сертификата пользователя или одиного из сертификатов цепочки"';
          CERT_TRUST_IS_NOT_TIME_NESTED : ErrorMsg :=
            '"Сертификаты в цепочке не корректно вложены"';
          CERT_TRUST_IS_REVOKED : ErrorMsg :=
            '"Доверие для сертификата пользователя или для одного из сертификатов цепочки было аннулировано"';
          CERT_TRUST_IS_NOT_SIGNATURE_VALID : ErrorMsg :=
            '"Сертификат пользователя или один из сертификатов цепочки не имеет действительной подписи"';
          CERT_TRUST_IS_NOT_VALID_FOR_USAGE : ErrorMsg :=
            '"Сертификат пользователя или один из сертификатов цепочки не подходит для предлагаемого использования"';
          CERT_TRUST_IS_UNTRUSTED_ROOT : ErrorMsg :=
            '"Сертификат корневого центра сертификации отсутствует в хранилище доверенных корневых центров сертификации"';
          CERT_TRUST_REVOCATION_STATUS_UNKNOWN : ErrorMsg :=
            '"Статус аннулирования сертификата пользователя или одного из сертификатов цепочки неизвестен"';
          CERT_TRUST_IS_CYCLIC : ErrorMsg :=
            '"Один из сертификатов цепочки выдан центром сертификации, который сертифицирован исходным сертификатом"';
          // Только для цепочек
          CERT_TRUST_IS_PARTIAL_CHAIN : ErrorMsg := '"Цепочка сертификатов не полная"';
          CERT_TRUST_CTL_IS_NOT_TIME_VALID : ErrorMsg :=
            '"У списка доверенных сертификатов, используемого для создания цепочки, истек срок действия"';
          CERT_TRUST_CTL_IS_NOT_SIGNATURE_VALID: ErrorMsg :=
            '"Список доверенных сертификатов, используемый для создания цепочки, не имеет действительной подписи"';
          CERT_TRUST_CTL_IS_NOT_VALID_FOR_USAGE: ErrorMsg :=
            '"Список доверенных сертификатов, используемый для создания цепочки, не подходит для предлагаемого использования"';
        else
          ErrorMsg := 'Код ошибки: 0x'
            + IntToHex( pChainElement^.TrustStatus.dwErrorStatus, 8 );
        end;
        if ( ErrorMsg <> '' ) then
          Break;
        Dec( pChainElement );
      end;
    end;
    if ErrorMsg <> '' then
      raise Exception.Create(
        'При проверке подлиности сертификата произошла ошибка: ' + #13#10 +
        ErrorMsg + '!' );
    SetLength( Result, pChainContext^.rgpChain^.cElement );
    pChainElement := @pChainContext.rgpChain^.rgpElement^;
    for i := 0 to pChainContext.rgpChain^.cElement - 1 do
    begin
      SetLength( Result[i], pChainElement^.pCertContext.cbCertEncoded );
      CopyMemory( Pointer( @Result[i][0] ),
        Pointer( pChainElement^.pCertContext.pbCertEncoded ),
        pChainElement^.pCertContext.cbCertEncoded );
      Inc( pChainElement );
    end;
  finally
    CertFreeCertificateChain( pChainContext );
  end;
  if not Assigned( Result ) then
    raise Exception.Create( 'Не удалось получить сертификаты цепочки!' );
end;

function TSetSign.Sign( hProv : HCRYPTPROV; KeyType : DWORD;
  const Buffer : TBytes ) : TSignInfo;
var
  hHash : HCRYPTHASH;
  HashData : TBytes;
  Size : DWORD;
begin
  Result.Sign := NIL;
  if Assigned( Buffer ) then
  begin
    if CryptCreateHash( hProv, AlgId, 0, 0, hHash ) then
    begin
      try
        Result.DateTime := Now();
        HashData := MergeBuffer( Pointer( @Buffer[0] ), Length( Buffer ),
          Pointer( @Result.DateTime ), SizeOf( TDateTime ) );
        if CryptHashData( hHash, @HashData[0], Length( HashData ), 0 ) then
        begin
          Size := 0;
          CryptSignHash( hHash, KeyType, NIL, 0, NIL, Size );
          if ( Size <> 0 ) then
          begin
            SetLength( Result.Sign, Size );
            if not CryptSignHash( hHash, KeyType, NIL, 0, @Result.Sign[0],
                Size ) then
              Result.Sign := NIL;
          end;
        end;
      finally
        CryptDestroyHash( hHash );
      end;
    end;
    if not Assigned( Result.Sign ) then
      raise Exception.Create( 'Не удалось подписать данные!' );
  end;
end;

function TSetSign.MergeBuffer( LeftBuf : Pointer; LeftBufSize : Integer;
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

function TSetSign.SingleSign( const Command : IReadCommand ) : Boolean;
var
  hProv : HCRYPTPROV;
  KeyInfo : TKeyInfo;
  ChainCerts : TBBytes;
  SignInfo : TSignInfo;
  CreateSignCmd : ICreateSignCommand;
begin
  Result := false;
  if Assigned( Command.Buffer ) then
  begin
    hProv := 0;
    KeyInfo.pCert := NIL;
    try
      hProv := InitCSP();
      KeyInfo := GetCertificate( hProv );
      ChainCerts := GetChainCerts( KeyInfo.pCert );
      SignInfo := Sign( hProv, KeyInfo.KeyType, MergeBuffer(
        Pointer( @Command.FileName[1] ),
        Length( Command.FileName ), Pointer( @Command.Buffer[0] ),
        Length( Command.Buffer ) ) );
      if Assigned( SignInfo.Sign ) then
      begin
        CreateSignCmd := TCreateSignCommand.Create( Command.FileName );
        CreateSignCmd.SignContext.CSPName := CSPName;
        CreateSignCmd.SignContext.AlgId := AlgId;
        CreateSignCmd.SignContext.DateTime := SignInfo.DateTime;
        CreateSignCmd.SignContext.Sign := SignInfo.Sign;
        CreateSignCmd.SignContext.Certificates := ChainCerts;
        Model.Execute( CreateSignCmd as ICommand );
        Result := true;
      end;
    finally
      if Assigned( KeyInfo.pCert ) then
        CertFreeCertificateContext( KeyInfo.pCert );
      if ( hProv <> 0 ) then
        CryptReleaseContext( hProv, 0 );
    end;
  end;
end;

end.
