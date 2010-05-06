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
    gbSignInfo: TGroupBox;
    aExit: TAction;
    miExit: TMenuItem;
    aOpen: TAction;
    mniFileSeparator2: TMenuItem;
    miOpen: TMenuItem;
    aViewCert: TAction;
    miViewCert: TMenuItem;
    miSignSeparator: TMenuItem;
    aSelectCheckSign: TAction;
    miCheckSign: TMenuItem;
    aDelSign: TAction;
    miDelSign: TMenuItem;
    aOpenAndCheckSign: TAction;
    aOpenAndSetSign: TAction;
    aSetting: TAction;
    miOpenAndCheckSign: TMenuItem;
    miOpenAndSetSign: TMenuItem;
    miSetting: TMenuItem;
    sbMain: TStatusBar;
    miFileSeparator1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aOpenAndSetSignExecute(Sender: TObject);
    procedure aOpenAndCheckSignExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aViewCertExecute(Sender: TObject);
    procedure aViewCertUpdate(Sender: TObject);
    procedure aCheckSignExecute(Sender: TObject);
    procedure aCheckSignUpdate(Sender: TObject);
    procedure aSelectCheckSignExecute(Sender: TObject);
    procedure aSelectCheckSignUpdate(Sender: TObject);
    procedure aSetSignExecute(Sender: TObject);
    procedure aSetSignUpdate(Sender: TObject);
    procedure aDelSignExecute(Sender: TObject);
    procedure aDelSignUpdate(Sender: TObject);
    procedure aSettingExecute(Sender: TObject);
    procedure lvFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

  private
    const
      SETSIGN_DLL = 'dcsetsign.dll';
      SETSIGN_FUNC = 'GetInstance';

      SIGN_VALID = 'ДЕЙСТВИТЕЛЬНА';
      SIGN_INVALID = 'НЕДЕЙСТВИТЕЛЬНА';

  private
    FileModel : IFileModel;
    CheckSign : ICheckSign;
    SetSign : IModule;

  public
    procedure Refresh( FilesInfo : TStringList ); overload;
    procedure Refresh( FileName : String; IsSign : Boolean = true ); overload;

  end;

var
  MainModule: TMainModule;

implementation

{$R *.dfm}

uses FileCtrl, uICommands, uFileModel, uCheckSign, uCommands;

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
    @GetInstance := GetProcAddress( SetSignDLL, SETSIGN_FUNC );
    if Assigned( @GetInstance ) then
      SetSign := GetInstance( FileModel as IModule )
    else
      FreeLibrary( SetSignDLL );
  end;
end;

procedure TMainModule.FormDestroy(Sender: TObject);
begin
  if Assigned( SetSign ) then
  begin
    SetSign := NIL;
    FreeLibrary( GetModuleHandle( SETSIGN_DLL ) );
  end;
  lvFiles.OnSelectItem := NIL;
end;

procedure TMainModule.FormShow(Sender: TObject);
begin
  if not Assigned( SetSign ) then
  begin
    aOpenAndSetSign.Visible := false;
    miSignSeparator.Visible := false;
    aSetSign.Visible := false;
    aDelSign.Visible := false;
    aSetting.Visible := false;
  end;
end;

procedure TMainModule.aOpenExecute(Sender: TObject);
var
  Dir : String;
begin
  if ( SelectDirectory( 'Выберите папку', '', Dir ) ) then
  begin
    FileModel.Open( Dir );
  end;
end;

procedure TMainModule.aOpenAndCheckSignExecute(Sender: TObject);
begin
//
end;

procedure TMainModule.aOpenAndSetSignExecute(Sender: TObject);
begin
//
end;

procedure TMainModule.aExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TMainModule.aViewCertExecute(Sender: TObject);
begin
  CheckSign.ViewCertificate( lvFiles.Items[lvFiles.ItemIndex].SubItems[0] );
end;

procedure TMainModule.aViewCertUpdate(Sender: TObject);
begin
  aViewCert.Enabled := ( lvFiles.SelCount = 1 ) and
    ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = 0 );
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
  if ( ( lvFiles.SelCount = 1 ) and
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

procedure TMainModule.aSelectCheckSignExecute(Sender: TObject);
begin
//
end;

procedure TMainModule.aSelectCheckSignUpdate(Sender: TObject);
begin
  aSelectCheckSign.Enabled := ( lvFiles.SelCount > 1 ) and
    ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = 0 );
end;

procedure TMainModule.aSetSignExecute(Sender: TObject);
begin
  SetSign.Execute( TSignCommand.Create(
    lvFiles.Items[lvFiles.ItemIndex].SubItems[0] ) as ICommand );
end;

procedure TMainModule.aSetSignUpdate(Sender: TObject);
begin
  aSetSign.Enabled := Assigned( SetSign ) and ( lvFiles.SelCount >= 1 ) and
      ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = -1 );
end;

procedure TMainModule.aDelSignExecute(Sender: TObject);
begin
  if ( IDYES =  Application.MessageBox(
      'Вы действительно хотите удалить подпись?', 'Удаление подписи',
      MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2 ) ) then
    if ( lvFiles.SelCount = 1 ) then
      FileModel.DeleteSign( lvFiles.Items[lvFiles.ItemIndex].SubItems[0] )
    else if ( lvFiles.SelCount > 1 ) then
    // MultiDeleteSign
end;

procedure TMainModule.aDelSignUpdate(Sender: TObject);
begin
  aDelSign.Enabled := Assigned( SetSign ) and ( lvFiles.SelCount >= 1 ) and
    ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = 0 );
end;

procedure TMainModule.aSettingExecute(Sender: TObject);
begin
//
end;

procedure TMainModule.lvFilesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  begin
    if ( lvFiles.SelCount = 1 ) then
      lvFiles.Tag := lvFiles.ItemIndex;
    if ( ( lvFiles.SelCount >= 1 ) and
        ( Item.ImageIndex = lvFiles.Items[lvFiles.Tag].ImageIndex ) ) then
      aCheckSign.Execute()
    else
      Item.Selected := false;
  end else if ( Item.ImageIndex = 0 ) then
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

procedure TMainModule.Refresh( FileName: String; IsSign : Boolean = true );
var
  i : Integer;
begin
  for i := 0 to lvFiles.Items.Count - 1 do
    if ( lvFiles.Items[i].SubItems.Strings[0] = FileName ) then
    begin
      if IsSign then
        lvFiles.Items[i].ImageIndex := 0
      else
        lvFiles.Items[i].ImageIndex := -1;
      Break;
    end;
  aCheckSign.Execute();
  lvFiles.Invalidate();
  if IsSign then
    MessageDlg( 'Файл успешно подписан!',  mtInformation, [mbOK], 0 )
  else
    MessageDlg( 'Подпись успешно удалена!',  mtInformation, [mbOK], 0 );
end;

end.
