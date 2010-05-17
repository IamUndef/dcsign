unit uMultiViewerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ImgList, ExtCtrls, ActnList, uIMultiViewer,
  uICheckSign;

type

  TMultiViewerForm = class(TForm)
    gbResult: TGroupBox;
    lvResult: TListView;
    ilMain: TImageList;
    bClose: TButton;
    pBottom: TPanel;
    alMain: TActionList;
    aClose: TAction;
    aViewCert: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure aCloseExecute(Sender: TObject);

  private
    const
      SLEEP_TIME = 10;

      PLEASE_WAIT = 'Пожалуйста, подождите...';

  private
    { Private declarations }
    CheckSign_ : ICheckSign;
    
  public
    { Public declarations }
    constructor Create( const CheckSign : ICheckSign );

    procedure Wait( IsOn : Boolean = true );

    procedure Show( Caption : String ); overload;
    procedure Hide( Msg : String );
    procedure AddFile( IsOk : Boolean; FileName : String; Result : String );

  end;

implementation

{$R *.dfm}

constructor TMultiViewerForm.Create( const CheckSign: ICheckSign );
begin
  inherited Create( NIL );
  CheckSign_ := CheckSign;
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

procedure TMultiViewerForm.Wait( IsOn : Boolean = true );
var
  Item : TListItem;
begin
  Enabled := not IsOn;
  if IsOn then
  begin
    Item := lvResult.Items.Add();
    Item.ImageIndex := -1;
    Item.SubItems.Add( PLEASE_WAIT )
  end else
    lvResult.Items.Delete( lvResult.Items.Count - 1 );
end;

procedure TMultiViewerForm.Show( Caption : String );
begin
  Self.Caption := Caption;
  Show();
  Wait();
  Application.ProcessMessages();
end;

procedure TMultiViewerForm.Hide( Msg : String );
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

procedure TMultiViewerForm.AddFile( IsOk : Boolean; FileName : String;
  Result : String );
var
  Item : TListItem;
begin
  Item := lvResult.Items.Insert( lvResult.Items.Count - 1 );
  Item.SubItems.Add( FileName );
  Item.SubItems.Add( Result );
  if IsOk then
    Item.ImageIndex := 0
  else
    Item.ImageIndex := 1;
  Application.ProcessMessages();
end;

end.
