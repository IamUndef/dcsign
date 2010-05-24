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
    procedure lvResultColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvResultCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvResultResize(Sender: TObject);

  private
    const
      SLEEP_TIME = 10;

      PLEASE_WAIT = 'Пожалуйста, подождите...';

  private
    { Private declarations }
    CheckSign_ : ICheckSign;

    ColIndex : Integer;
    ArrowType : Integer;

    procedure SetColumnArrow( ColIndex : Integer; ArrowType : Integer );

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

uses CommCtrl;

constructor TMultiViewerForm.Create( FileIconList : TFileIconList;
  const CheckSign: ICheckSign );
begin
  inherited Create( NIL );
  CheckSign_ := CheckSign;
  lvResult.SmallImages := FileIconList;
  aViewCert.Visible := Assigned( CheckSign_ );
  ColIndex := 0;
  ArrowType := 0;
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

procedure TMultiViewerForm.lvResultColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if ( ColIndex <> Column.Index ) then
  begin
    SetColumnArrow( ColIndex, 0 );
    ColIndex := Column.Index;
    ArrowType := HDF_SORTDOWN;
  end;
  if ( ( ArrowType = 0 ) or ( ArrowType = HDF_SORTDOWN ) ) then
    ArrowType := HDF_SORTUP
  else
    ArrowType := HDF_SORTDOWN;
  lvResult.AlphaSort();
  SetColumnArrow( ColIndex, ArrowType );
end;

procedure TMultiViewerForm.lvResultCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  if ( ColIndex = 0 ) then
    Compare := AnsiCompareText( Item1.Caption, Item2.Caption )
  else
    Compare := AnsiCompareText( Item1.SubItems[ColIndex - 1],
      Item2.SubItems[ColIndex - 1] );
  if ( ArrowType = HDF_SORTDOWN ) then
    Compare := -Compare;    
end;

procedure TMultiViewerForm.lvResultResize(Sender: TObject);
begin
  SetColumnArrow( ColIndex, ArrowType );
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
  lvResult.ItemIndex := lvResult.Items.Count - 1;
  lvResult.Items[lvResult.ItemIndex].MakeVisible( false );
  Application.ProcessMessages();
end;

procedure TMultiViewerForm.SetColumnArrow( ColIndex : Integer;
  ArrowType : Integer );
var
 hHeader : HWND;
 hdItem : THDItem;
begin
  ZeroMemory( Pointer( @hdItem ), SizeOf( THDItem ) );
  hHeader := ListView_GetHeader( lvResult.Handle );
  hdItem.Mask := HDI_FORMAT;
  Header_GetItem( hHeader, ColIndex, hdItem );
  hdItem.fmt := hdItem.fmt and not ( HDF_SORTDOWN or HDF_SORTUP );
  if ( ArrowType <> 0 ) then
    hdItem.fmt := hdItem.fmt or ArrowType;
  Header_SetItem( hHeader, ColIndex, hdItem );  
end;

end.
