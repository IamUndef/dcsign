unit uSelectContainer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ActnList, ImgList, JwaWinCrypt;

type
  TSelectContainer = class(TForm)
    cbContainer: TComboBox;
    gbContainer: TGroupBox;
    bbRefresh: TBitBtn;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    alMain: TActionList;
    ilMain: TImageList;
    aRefresh: TAction;
    aOk: TAction;
    aCancel: TAction;
    procedure FormShow(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure aOkUpdate(Sender: TObject);
    procedure aCancelExecute(Sender: TObject);

  private
    { Private declarations }
    hProv : PHCRYPTPROV;
    Container_ : String; 

    function GetContainer() : String;

  public
    { Public declarations }
    constructor Create( CryptProvider : PHCRYPTPROV ); reintroduce;

    property Container : String read GetContainer write Container_;

  end;

implementation

{$R *.dfm}

constructor TSelectContainer.Create( CryptProvider : PHCRYPTPROV );
begin
  inherited Create( NIL );
  hProv := CryptProvider;
end;

procedure TSelectContainer.FormShow(Sender: TObject);
begin
  aRefresh.Tag := 1;
  aRefresh.Execute;
  aRefresh.Tag := 0;
end;

procedure TSelectContainer.aRefreshExecute(Sender: TObject);
var
  SelContainer : String;
  Size : DWORD;
  Flag : DWORD;
  Data : String;
begin
  SelContainer := Container_;
  cbContainer.Clear;
  if CryptGetProvParam( hProv^, PP_ENUMCONTAINERS, NIL, Size, CRYPT_FIRST ) then
  begin
    SetLength( Data, Size );
    Flag := CRYPT_FIRST;
    while CryptGetProvParam( hProv^, PP_ENUMCONTAINERS, @Data[1], Size, Flag ) do
    begin
      if ( Trim( Data ) <> '' ) then
        cbContainer.Items.Add( Data );
      Flag := CRYPT_NEXT;
    end;
  end;
  if ( cbContainer.Items.Count <> 0 ) then
  begin
    cbContainer.ItemIndex := cbContainer.Items.IndexOf( SelContainer );
    if ( cbContainer.ItemIndex = -1 ) then
      cbContainer.ItemIndex := 0;
  end;
  if ( aRefresh.Tag = 0 ) then
    MessageDlg( 'Список контейнеров обновлен!',  mtInformation, [mbOK], 0 );
end;

procedure TSelectContainer.aOkUpdate(Sender: TObject);
begin
  aOk.Enabled := cbContainer.ItemIndex <> -1;
end;

procedure TSelectContainer.aCancelExecute(Sender: TObject);
begin
  cbContainer.ItemIndex := -1;
end;

function TSelectContainer.GetContainer() : String;
begin
  if ( cbContainer.ItemIndex <> -1 ) then
    Result := cbContainer.Items[cbContainer.ItemIndex]
  else
    Result := Container_;
end;

end.
