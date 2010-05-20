unit uMultiViewerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ImgList, ExtCtrls, ActnList, ToolWin,
  uIMultiViewer, uICheckSign, uFileIconList;

type

  TMultiViewerForm = class(TForm)
    gbResult: TGroupBox;
    lvResult: TListView;
    ilMain: TImageList;
    alMain: TActionList;
    aClose: TAction;
    aViewCert: TAction;
    tbMain: TToolBar;
    tbClose: TToolButton;
    tbViewCert: TToolButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure aCloseExecute(Sender: TObject);
    procedure aViewCertExecute(Sender: TObject);

  private
    const
      SLEEP_TIME = 10;

      PLEASE_WAIT = '����������, ���������...';

  private
    { Private declarations }
    CheckSign_ : ICheckSign;

  public
    { Public declarations }
    constructor Create( FileIconList : TFileIconList;
      const CheckSign : ICheckSign ); reintroduce;

    procedure Wait( IsOn : Boolean = true );

    procedure Show( const Caption : String ); overload;
    procedure Hide( const Msg : String );
    procedure AddFile( IsOk : Boolean; const FileName : String;
      const Result : String );

  end;

implementation

{$R *.dfm}

constructor TMultiViewerForm.Create( FileIconList : TFileIconList;
  const CheckSign: ICheckSign );
begin
  inherited Create( NIL );
  CheckSign_ := CheckSign;
  lvResult.SmallImages := FileIconList;
  aViewCert.Visible := Assigned( CheckSign_ );
end;

procedure TMultiViewerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ( Tag = 0 ) then
    Action := caNone;
end;

procedure TMultiViewerForm.aCloseExecute(Sender: TObject);
begin
  Close();
end;

procedure TMultiViewerForm.aViewCertExecute(Sender: TObject);
begin
  if ( lvResult.ItemIndex <> -1 ) then
    CheckSign_.ViewCertificate( lvResult.Items[lvResult.ItemIndex].Caption );
end;

procedure TMultiViewerForm.Wait( IsOn : Boolean = true );
var
  Item : TListItem;
begin
  Enabled := not IsOn;
  if IsOn then
  begin
    Item := lvResult.Items.Add();
    Item.Caption := PLEASE_WAIT;
    Item.ImageIndex := -1;
  end else
    lvResult.Items.Delete( lvResult.Items.Count - 1 );
end;

procedure TMultiViewerForm.Show( const Caption : String );
begin
  Self.Caption := Caption;
  Show();
  Wait();
  Application.ProcessMessages();
end;

procedure TMultiViewerForm.Hide( const Msg : String );
begin
  Tag := 1;
  Wait( false );
  MessageDlg( Msg,  mtInformation, [mbOK], 0 );
  while Visible do
  begin
    Application.ProcessMessages();
    Sleep( SLEEP_TIME );
  end;
end;

procedure TMultiViewerForm.AddFile( IsOk : Boolean; const FileName : String;
  const Result : String );
var
  Item : TListItem;
begin
  Item := lvResult.Items.Insert( lvResult.Items.Count - 1 );
  Item.Caption := FileName;
  Item.SubItems.Add( Result );
  Item.ImageIndex :=
    ( lvResult.SmallImages as TFileIconList ).IndexOf( FileName );
  if IsOk then
    Item.StateIndex := 0
  else
    Item.StateIndex := 1;
  Application.ProcessMessages();
end;

end.
