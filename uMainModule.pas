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
    pmMain: TPopupMenu;
    miViewCertPopup: TMenuItem;
    miSelectCheckSignPopup: TMenuItem;
    miSetSignPopup: TMenuItem;
    miDelSignPopup: TMenuItem;
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

      UNSIGN_IMAGE_INDEX = -1;
      SIGN_IMAGE_INDEX = 0;
      
      SIGN_VALID = 'ДЕЙСТВИТЕЛЬНА';
      SIGN_INVALID = 'НЕДЕЙСТВИТЕЛЬНА';

  private
    type
      TRefreshType = ( rtOpen, rtSign, rtDelete );

  private
    FileModel : IFileModel;
    CheckSign : ICheckSign;
    SetSign : IModule;

    procedure GetSelectedFiles( Files : TStrings );

  public
    procedure Refresh( Files : TStrings; RefreshType : TRefreshType = rtOpen );

  end;

var
  MainModule: TMainModule;

implementation

{$R *.dfm}

uses
  FileCtrl, uICommands, uIMultiViewer, uFileModel, uCheckSign, uCommands,
  uMultiViewer;

procedure TMainModule.FormCreate(Sender: TObject);

type
  TGetInstance = function ( const FileModel : IModule ) : IModule;
  
var
  SetSignDLL : THandle;
  GetInstance : TGetInstance;

begin
  FileModel := TFileModel.Create();
  CheckSign := TCheckSign.Create( FileModel );
  SetSignDLL := LoadLibrary( SETSIGN_DLL );
  if ( SetSignDLL <> 0 ) then
  begin
    @GetInstance := GetProcAddress( SetSignDLL, SETSIGN_FUNC );
    if Assigned( @GetInstance ) then
    begin
      SetSign := GetInstance( FileModel as IModule );
      if not Assigned( SetSign ) then
        MessageDlg( 'Не удалось инициализировать модуль подписи!',
          mtError, [mbOK], 0 );
    end
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
  Files : TStringList;
begin
  if ( SelectDirectory( 'Выберите папку', '', Dir ) ) then
  begin
    Files := NIL;
    try
      Files := TStringList.Create();
      FileModel.Open( Dir, Files );
      Refresh( Files );
    finally
      if Assigned( Files ) then
        Files.Free();
    end;
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
    ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = SIGN_IMAGE_INDEX );
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
      ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = SIGN_IMAGE_INDEX ) ) then
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
var
  Files : TStringList;
begin
  Enabled := false;
  Files := NIL;
  try
    Files := TStringList.Create();
    GetSelectedFiles( Files );
    CheckSign.MultiCheck( TMultiViewer.Create( CheckSign ) as IMultiViewer,
      Files );
  finally
    Enabled := true;
    if Assigned( Files ) then
      Files.Free();
  end;
end;

procedure TMainModule.aSelectCheckSignUpdate(Sender: TObject);
begin
  aSelectCheckSign.Enabled := ( lvFiles.SelCount > 1 ) and
    ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = SIGN_IMAGE_INDEX );
end;

procedure TMainModule.aSetSignExecute(Sender: TObject);
var
  SignCmd : ISignCommand;
  MultiSignCmd : IMultiSignCommand;
begin
  Enabled := false;
  try
    if ( lvFiles.SelCount = 1 ) then
    begin
      SignCmd := TSignCommand.Create(
        lvFiles.Items[lvFiles.ItemIndex].SubItems[0] );
      if not SetSign.Execute( SignCmd as ICommand ) then
        MessageDlg( SignCmd.ExceptionMsg,  mtError, [mbOK], 0 )
      else
      begin
        lvFiles.Items[lvFiles.ItemIndex].ImageIndex := SIGN_IMAGE_INDEX;
        aCheckSign.Execute();
        MessageDlg( 'Файл успешно подписан!',  mtInformation, [mbOK], 0 )
      end;
    end
    else if ( lvFiles.SelCount > 1 ) then
    begin
      MultiSignCmd := TMultiSignCommand.Create();
      GetSelectedFiles( MultiSignCmd.Files );
      if not SetSign.Execute( MultiSignCmd as ICommand ) then
        MessageDlg( MultiSignCmd.ExceptionMsg,  mtError, [mbOK], 0 )
      else
        Refresh( MultiSignCmd.Files, rtSign );
    end;
  finally
    Enabled := true;
  end;
end;

procedure TMainModule.aSetSignUpdate(Sender: TObject);
begin
  aSetSign.Enabled := Assigned( SetSign ) and ( lvFiles.SelCount >= 1 ) and
      ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = UNSIGN_IMAGE_INDEX );
end;

procedure TMainModule.aDelSignExecute(Sender: TObject);
var
  Files : TStringList;
begin
  if ( lvFiles.SelCount = 1 ) then
  begin
    if ( IDYES = Application.MessageBox(
        'Вы действительно хотите удалить подпись?', 'Удаление подписи',
        MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2 ) ) then
    begin
      FileModel.SingleDeleteSign(
        lvFiles.Items[lvFiles.ItemIndex].SubItems[0] );
      lvFiles.Items[lvFiles.ItemIndex].ImageIndex := UNSIGN_IMAGE_INDEX;
      aCheckSign.Execute();
      MessageDlg( 'Подпись успешно удалена!',  mtInformation, [mbOK], 0 );
    end;
  end
  else if ( lvFiles.SelCount > 1 ) then
  begin
    if ( IDYES = Application.MessageBox(
        'Вы действительно хотите удалить подписи?', 'Удаление подписей',
        MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2 ) ) then
    begin
      Enabled := false;
      Files := NIL;
      try
        Files := TStringList.Create();
        GetSelectedFiles( Files );
        FileModel.MultiDeleteSign( TMultiViewer.Create() as IMultiViewer,
          Files );
        Refresh( Files, rtDelete );
      finally
        Enabled := true;
        if Assigned( Files ) then
          Files.Free();
      end;
    end;
  end;
end;

procedure TMainModule.aDelSignUpdate(Sender: TObject);
begin
  aDelSign.Enabled := Assigned( SetSign ) and ( lvFiles.SelCount >= 1 ) and
    ( lvFiles.Items[lvFiles.ItemIndex].ImageIndex = SIGN_IMAGE_INDEX );
end;

procedure TMainModule.aSettingExecute(Sender: TObject);
begin
  SetSign.Execute( TSettingCommand.Create() as ICommand );
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
  end else if ( Item.ImageIndex = SIGN_IMAGE_INDEX ) then
    aCheckSign.Execute();
end;

procedure TMainModule.Refresh( Files: TStrings;
  RefreshType : TRefreshType = rtOpen  );
var
  i : Integer;
  Item : TListItem;
begin
  if ( RefreshType = rtOpen ) then
  begin
    lvFiles.Clear;
    for i := 0 to Files.Count - 1 do
    begin
      Item := lvFiles.Items.Add();
      if Assigned( Files.Objects[i] ) then
        Item.ImageIndex := SIGN_IMAGE_INDEX
      else
        Item.ImageIndex := UNSIGN_IMAGE_INDEX;
      Item.SubItems.Add( Files.Strings[i] );
    end;
  end else
  begin
    lvFiles.OnSelectItem := NIL;
    try
      Item := lvFiles.Selected;
      while ( Item <> NIl ) do
      begin
        if ( Files.IndexOf( Item.SubItems[0] ) <> - 1 ) then
        begin
          if ( RefreshType = rtSign ) then
            Item.ImageIndex := SIGN_IMAGE_INDEX
          else
            Item.ImageIndex := UNSIGN_IMAGE_INDEX;
        end else
          Item.Selected := false;
        Item := lvFiles.GetNextItem( Item, sdAll, [isSelected] );
      end;
    finally
      lvFiles.OnSelectItem := lvFilesSelectItem;
    end;
    aCheckSign.Execute();
  end;
end;

procedure TMainModule.GetSelectedFiles( Files : TStrings );
var
  Item : TListItem;
begin
  Item := lvFiles.Selected;
  while ( Item <> NIl ) do
  begin
    Files.Add( Item.SubItems[0] );
    Item := lvFiles.GetNextItem( Item, sdAll, [isSelected] );
  end;
end;

end.

