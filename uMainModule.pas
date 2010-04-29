unit uMainModule;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ComCtrls, ImgList, Menus, ActnList, ExtCtrls,
  uIFileModel, uICheckSign, uIModule;

type
  TMainModule = class(TForm)
    gbMain: TGroupBox;
    lvFiles: TListView;
    ilMain: TImageList;
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miSign: TMenuItem;
    miSetSign: TMenuItem;
    alMain: TActionList;
    aSetSign: TAction;
    aCheckSign: TAction;
    lbSignTitle: TLabel;
    lbSign: TLabel;
    lbDateTimeTitle: TLabel;
    lbDateTime: TLabel;
    lbSubjectTitle: TLabel;
    mSubject: TMemo;
    GroupBox1: TGroupBox;
    aExit: TAction;
    miExit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aSetSignExecute(Sender: TObject);
    procedure aSetSignUpdate(Sender: TObject);
    procedure aCheckSignExecute(Sender: TObject);
    procedure aCheckSignUpdate(Sender: TObject);
    procedure lvFilesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);

  private
    const
      SETSIGN_DLL = 'dcsetsign.dll';
      SIGN_VALID = 'ƒ≈…—“¬»“≈À‹Õ¿';
      SIGN_INVALID = 'Õ≈ƒ≈…—“¬»“≈À‹Õ¿';

  private
    FileModel : IFileModel;
    CheckSign : ICheckSign;
    SetSign : IModule;

  public
    procedure Refresh( FilesInfo : TStringList ); overload;
    procedure Refresh( FileName : String ); overload;

  end;

var
  MainModule: TMainModule;

implementation

{$R *.dfm}

uses uICommands, uFileModel, uCheckSign, uCommands;

procedure TMainModule.FormCreate(Sender: TObject);

type
  TGetInstance = function ( const FileModel : IModule ) : IModule;
  
var
  SetSignDLL : THandle;
  GetInstance : TGetInstance;

begin
  FileModel := TFileModel.Create( Self );
  CheckSign := TCheckSign.Create( FileModel );
  SetSignDLL := LoadLibrary( SETSIGN_DLL );
  if ( SetSignDLL <> 0 ) then
  begin
    @GetInstance := GetProcAddress( SetSignDLL, 'GetInstance' );
    if Assigned( @GetInstance ) then
      SetSign := GetInstance( FileModel as IModule )
    else
      FreeLibrary( SetSignDLL );
  end;
end;

procedure TMainModule.FormDestroy(Sender: TObject);
begin
  FileModel := NIL;
  if Assigned( SetSign ) then
  begin
    SetSign := NIL;
    FreeLibrary( GetModuleHandle( SETSIGN_DLL ) );
  end;
end;

procedure TMainModule.FormShow(Sender: TObject);
begin
  FileModel.Open( 'c:\' );
end;

procedure TMainModule.aExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TMainModule.aSetSignExecute(Sender: TObject);
begin
  if ( Assigned( SetSign ) and
      ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = -1 ) ) then
    SetSign.Execute( TSignCommand.Create(
      lvFiles.Items[lvFiles.ItemIndex].SubItems[0] ) as ICommand );
end;

procedure TMainModule.aSetSignUpdate(Sender: TObject);
begin
  if ( ( lvFiles.ItemIndex <> -1 ) and
      ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = -1 ) ) then
    aSetSign.Enabled := true
  else
    aSetSign.Enabled := false;
end;

procedure TMainModule.aCheckSignExecute(Sender: TObject);
begin
  if CheckSign.SingleCheck( lvFiles.Items[lvFiles.ItemIndex].SubItems[0] ) then
  begin
    lbSign.Font.Color := clGreen;
    lbSign.Caption := SIGN_VALID;
  end else
  begin
    lbSign.Font.Color := clRed;
    lbSign.Caption := SIGN_INVALID;
  end;
  if ( CheckSign.DateTime <> 0 ) then
    lbDateTime.Caption := DateTimeToStr( CheckSign.DateTime )
  else
    lbDateTime.Caption := '';
  if ( CheckSign.CertSubject <> '' ) then
    mSubject.Text := CheckSign.CertSubject
  else
    mSubject.Text := '';
end;

procedure TMainModule.aCheckSignUpdate(Sender: TObject);
begin
  if ( ( lvFiles.ItemIndex <> -1 ) and
      ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = 0 ) ) then
    aCheckSign.Enabled := true
  else
  begin
    aCheckSign.Enabled := false;
    lbSign.Caption := '';
    lbDateTime.Caption := '';
    mSubject.Text := '';
  end;
end;

procedure TMainModule.lvFilesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  aCheckSign.Execute();
end;

procedure TMainModule.Refresh( FilesInfo : TStringList );
var
  i : Integer;
  Item : TListItem;
begin
  lvFiles.Clear;
  for i := 0 to FilesInfo.Count - 1 do
  begin
    Item := lvFiles.Items.Add();
    if Assigned( FilesInfo.Objects[i] ) then
      Item.ImageIndex := 0
    else
      Item.ImageIndex := -1;
    Item.SubItems.Add( FilesInfo.Strings[i] );
  end;
end;

procedure TMainModule.Refresh( FileName: String );
var
  i : Integer;
begin
  for i := 0 to lvFiles.Items.Count - 1 do
  begin
    if ( lvFiles.Items[i].SubItems.Strings[0] = FileName ) then
    begin
      lvFiles.Items[i].ImageIndex := 0;
      Break;
    end;
  end;
  aCheckSign.Execute();
  lvFiles.Invalidate();
end;

end.
