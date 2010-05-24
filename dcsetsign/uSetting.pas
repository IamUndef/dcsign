unit uSetting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ImgList, ActnList;

type
  TSetting = class(TForm)
    gbCSP: TGroupBox;
    cbCSP: TComboBox;
    gbAlg: TGroupBox;
    cbAlg: TComboBox;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    alMain: TActionList;
    aOk: TAction;
    aCancel: TAction;
    ilMain: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbCSPChange(Sender: TObject);
    procedure aOkUpdate(Sender: TObject);
    procedure aCancelExecute(Sender: TObject);

  private
    { Private declarations }
    CSPName_ : String;
    AlgId_ : Integer;

    function GetCSP() : String;
    function GetAlgId() : Integer;

  public
    { Public declarations }

    property
      CSPName : String read GetCSP write CSPName_;
    property
      AlgId : Integer read GetAlgId write AlgId_;

  end;

implementation

{$R *.dfm}

uses JwaWinCrypt;

procedure TSetting.FormCreate(Sender: TObject);
begin
  CSPName_ := '';
  AlgId_ := 0;
end;

procedure TSetting.FormShow(Sender: TObject);
var
  Index : Integer;
  ProvType : DWORD;
  Size : DWORD;
  Data : String;
begin
  Index := 0;
  while ( CryptEnumProviders( Index, NIL, 0, ProvType, NIL, Size ) ) do
  begin
    SetLength( Data, Size );
		if ( CryptEnumProviders( Index, NIL, 0, ProvType, PChar( Data ), Size ) and
        ( Trim( Data ) <> '' ) ) then
      cbCSP.Items.AddObject( Data, TObject( ProvType ) );
    Inc( Index );
  end;    
  if ( cbCSP.Items.Count <> 0 ) then
  begin
    cbCSP.ItemIndex := cbCSP.Items.IndexOf( CSPName_ );
    if ( cbCSP.ItemIndex = -1 ) then
      cbCSP.ItemIndex := 0;
    cbCSP.OnChange( cbCSP );
  end else
  begin
    cbCSP.Enabled := false;
    cbAlg.Enabled := false;
  end;
end;

procedure TSetting.cbCSPChange(Sender: TObject);
var
  hProv : HCRYPTPROV;
  Size : DWORD;
  Flag : DWORD;
  Data : TBytes;
  AlgInfo : PPROV_ENUMALGS_EX;
begin
  if ( cbCSP.ItemIndex <> -1 ) then
  begin
    hProv := 0;
    cbAlg.Clear;
    if CryptAcquireContext( hProv, '',
        PChar( cbCSP.Items[cbCSP.ItemIndex] ),
        Integer( cbCSP.Items.Objects[cbCSP.ItemIndex] ),
        CRYPT_VERIFYCONTEXT ) then
    begin
      try
        if CryptGetProvParam( hProv, PP_ENUMALGS_EX, NIL, Size,
            CRYPT_FIRST ) then
        begin
          SetLength( Data, Size );
          Flag := CRYPT_FIRST;
          while CryptGetProvParam( hProv, PP_ENUMALGS_EX, @Data[0], Size,
              Flag ) do
          begin
            AlgInfo := PPROV_ENUMALGS_EX( @Data[0] );
            if ( GET_ALG_CLASS( AlgInfo.aiAlgid ) = ALG_CLASS_HASH ) then
              cbAlg.Items.AddObject( AlgInfo.szLongName,
                TObject( AlgInfo.aiAlgid ) );
            Flag := CRYPT_NEXT;
          end;
        end;
      finally
        CryptReleaseContext( hProv, 0 );
      end;  
    end else
      MessageDlg( 'Не удалось инициализировать криптопровайдер - "' +
        cbCSP.Items[cbCSP.ItemIndex] + '"!',  mtError, [mbOK], 0 );
    if ( cbAlg.Items.Count <> 0 ) then
    begin
      cbAlg.Enabled := true;
      if ( CSPName_ = cbCSP.Items[cbCSP.ItemIndex] ) then
        cbAlg.ItemIndex := cbAlg.Items.IndexOfObject( TObject( AlgId_ ) );
      if ( cbAlg.ItemIndex = -1 ) then
        cbAlg.ItemIndex := 0;
    end else
      cbAlg.Enabled := false;
  end;
end;

procedure TSetting.aOkUpdate(Sender: TObject);
begin
  aOk.Enabled := ( cbCSP.ItemIndex <> -1 ) and ( cbAlg.ItemIndex <> -1 );
end;

procedure TSetting.aCancelExecute(Sender: TObject);
begin
  cbCSP.ItemIndex := -1;
  cbAlg.ItemIndex := -1;
end;

function TSetting.GetCSP() : String;
begin
  if ( cbCSP.ItemIndex <> -1 ) then
    Result := cbCSP.Items[cbCSP.ItemIndex]
  else
    Result := CSPName_;
end;

function TSetting.GetAlgId() : Integer;
begin
  if ( cbAlg.ItemIndex <> -1 ) then
    Result := Integer( cbAlg.Items.Objects[cbAlg.ItemIndex] )
  else
    Result := AlgId_;
end;

end.
