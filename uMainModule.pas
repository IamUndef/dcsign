unit uMainModule;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ComCtrls, ImgList, Menus, ActnList, ExtCtrls,
  uIFileModel, uICheckSign, uIModule, uFileIconList;

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
    miFileSeparator2: TMenuItem;
    miOpen: TMenuItem;
    aViewCert: TAction;
    miViewCert: TMenuItem;
    miSignSeparator1: TMenuItem;
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
    aChangeContainer: TAction;
    miChangeContainer: TMenuItem;
    miSignSeparator2: TMenuItem;
    aAbout: TAction;
    miHelp: TMenuItem;
    miAbout: TMenuItem;
    aCopy: TAction;
    miSeparatorPopup: TMenuItem;
    miCopyPopup: TMenuItem;
    miFileSeparator3: TMenuItem;
    miCopy: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aOpenAndCheckSignExecute(Sender: TObject);
    procedure aOpenAndSetSignExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aCopyUpdate(Sender: TObject);
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
    procedure aChangeContainerExecute(Sender: TObject);
    procedure aSettingExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure lvFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvFilesDblClick(Sender: TObject);
    procedure lvFilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvFilesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvFilesResize(Sender: TObject);

  private
    const
      SETSIGN_DLL = 'dcsetsign.dll';
      SETSIGN_FUNC = 'GetInstance';
      DCSIGN_SETUP_EXE = 'dcsignSetup.exe';

      UNSIGN_IMAGE_INDEX = -1;
      SIGN_IMAGE_INDEX = 0;
      
      SIGN_VALID = '�������������';
      SIGN_INVALID = '���������������';

  private
    type
      TRefreshType = ( rtOpen, rtSign, rtDelete );

  private
    FileModel : IFileModel;
    CheckSign : ICheckSign;
    SetSign : IModule;
    FileIconList : TFileIconList;

    ArrowType : Integer;

    procedure GetSelectedFiles( Files : TStrings );

  public
    procedure Refresh( Files : TStrings; RefreshType : TRefreshType = rtOpen );
    procedure SetColumnArrow( ColIndex : Integer; ArrowType : Integer );

  end;

var
  MainModule: TMainModule;

implementation

{$R *.dfm}

uses
  FileCtrl, ShellAPI, CommCtrl, uICommands, uIMultiViewer, uFileModel,
  uCheckSign, uCommands, uMultiViewer, uAbout;

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
        MessageDlg( '�� ������� ���������������� ������ �������!', mtError,
          [mbOK], 0 );
    end
    else
      FreeLibrary( SetSignDLL );
  end;
  FileIconList := TFileIconList.Create( Self );
  lvFiles.SmallImages := FileIconList;
  ArrowType := HDF_SORTUP;
end;

procedure TMainModule.FormDestroy(Sender: TObject);
begin
  lvFiles.OnSelectItem := NIL;
  lvFiles.SmallImages := NIL;
  if Assigned( SetSign ) then
  begin
    SetSign := NIL;
    FreeLibrary( GetModuleHandle( SETSIGN_DLL ) );
  end;
  if Assigned( FileIconList ) then
    FileIconList.Free();
end;

procedure TMainModule.FormShow(Sender: TObject);
begin
  if not Assigned( SetSign ) then
  begin
    aOpenAndSetSign.Visible := false;
    aCopy.Visible := false;
    aSetSign.Visible := false;
    aDelSign.Visible := false;
    aChangeContainer.Visible := false;
    aSetting.Visible := false;
    miFileSeparator3.Visible := false;
    miSignSeparator1.Visible := false;
    miSignSeparator2.Visible := false;
    miSeparatorPopup.Visible := false;
  end;
  SetColumnArrow( 0, ArrowType );
end;

procedure TMainModule.aOpenExecute(Sender: TObject);
var
  Dir : String;
  Files : TStringList;
begin
  aOpen.Tag := 0;
  if SelectDirectory( '�������� �����', '', Dir ) then
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
    aOpen.Tag := 1;
  end;
end;

procedure TMainModule.aOpenAndCheckSignExecute(Sender: TObject);
var
  i : Integer;
begin
  aOpen.Execute();
  if ( aOpen.Tag = 1 ) then
  begin
    lvFiles.OnSelectItem := NIL;
    try
      for i := 0 to lvFiles.Items.Count - 1 do
        if ( lvFiles.Items[i].StateIndex = SIGN_IMAGE_INDEX ) then
          lvFiles.Items[i].Selected := true;
    finally
     lvFiles.OnSelectItem := lvFilesSelectItem;
    end;
    if ( lvFiles.SelCount = 1 ) then
      aCheckSign.Execute()
    else if ( lvFiles.SelCount > 1 ) then
      aSelectCheckSign.Execute();
  end;
end;

procedure TMainModule.aOpenAndSetSignExecute(Sender: TObject);
var
  i : Integer;
begin
  aOpen.Execute();
  if ( aOpen.Tag = 1 ) then
  begin
    lvFiles.OnSelectItem := NIL;
    try
      for i := 0 to lvFiles.Items.Count - 1 do
        if ( lvFiles.Items[i].StateIndex = UNSIGN_IMAGE_INDEX ) then
          lvFiles.Items[i].Selected := true;
    finally
      lvFiles.OnSelectItem := lvFilesSelectItem;
    end;
    aSetSign.Execute();
  end;
end;

procedure TMainModule.aCopyExecute(Sender: TObject);
var
  Dir : String;
  ErrorMsg : String;
  Files : TStringList;
begin
  if SelectDirectory( '�������� ����� ��� �����������', '', Dir,
      [sdNewUI, sdNewFolder] ) then
  begin
    if not CopyFile(
        PChar( ExtractFilePath( Application.ExeName ) + DCSIGN_SETUP_EXE ),
        PChar( Dir + '\' + DCSIGN_SETUP_EXE ), false ) then
    begin
      ErrorMsg := SysErrorMessage( GetLastError() );
      MessageDlg( Format(
          '�� ������� ����������� ������������ ����� "%s": %s!',
          [ExtractFilePath( Application.ExeName ) + DCSIGN_SETUP_EXE,
            ErrorMsg] ),
        mtError, [mbOK], 0 );
    end
    else
    begin
      if ( lvFiles.SelCount = 1 ) then
      begin
        FileModel.SingleCopy( Dir, lvFiles.Items[lvFiles.ItemIndex].Caption );
        MessageDlg( '���� ������� ����������!', mtInformation, [mbOK], 0 );
      end else if ( lvFiles.SelCount > 1 ) then
      begin
        Files := NIL;
        try
          Files := TStringList.Create();
          GetSelectedFiles( Files );
          FileModel.MultiCopy(
            TMultiViewer.Create( FileIconList ) as IMultiViewer, Dir, Files );
        finally
          if Assigned( Files ) then
            Files.Free();
        end;
      end;
    end;
  end;
end;

procedure TMainModule.aCopyUpdate(Sender: TObject);
begin
  aCopy.Enabled := Assigned( SetSign ) and ( lvFiles.SelCount > 0 ) and
    ( lvFiles.Items[lvFiles.ItemIndex].StateIndex = SIGN_IMAGE_INDEX );
end;

procedure TMainModule.aExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TMainModule.aViewCertExecute(Sender: TObject);
begin
  CheckSign.ViewCertificate( lvFiles.Items[lvFiles.ItemIndex].Caption );
end;

procedure TMainModule.aViewCertUpdate(Sender: TObject);
begin
  aViewCert.Enabled := ( lvFiles.SelCount = 1 ) and
    ( lvFiles.Items[lvFiles.ItemIndex].StateIndex = SIGN_IMAGE_INDEX );
end;

procedure TMainModule.aCheckSignExecute(Sender: TObject);
begin
  if CheckSign.SingleCheck( lvFiles.Items[lvFiles.ItemIndex].Caption ) then
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
      ( lvFiles.Items[lvFiles.ItemIndex].StateIndex = SIGN_IMAGE_INDEX ) ) then
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
    CheckSign.MultiCheck(
      TMultiViewer.Create( FileIconList, CheckSign ) as IMultiViewer, Files );
  finally
    Enabled := true;
    if Assigned( Files ) then
      Files.Free();
  end;
end;

procedure TMainModule.aSelectCheckSignUpdate(Sender: TObject);
begin
  aSelectCheckSign.Enabled := ( lvFiles.SelCount > 1 ) and
    ( lvFiles.Items[lvFiles.ItemIndex].StateIndex = SIGN_IMAGE_INDEX );
end;

procedure TMainModule.aSetSignExecute(Sender: TObject);
var
  Cmd : ICommand;
begin
  Enabled := false;
  try
    if ( lvFiles.SelCount = 1 ) then
    begin
      Cmd := TSignCommand.Create(
        lvFiles.Items[lvFiles.ItemIndex].Caption ) as ICommand;
      if not SetSign.Execute( Cmd ) then
      begin
        if Cmd.IsException() then
          MessageDlg( Cmd.ExceptionMsg, mtError, [mbOK], 0 );
      end
      else
      begin
        lvFiles.Items[lvFiles.ItemIndex].StateIndex := SIGN_IMAGE_INDEX;
        aCheckSign.Execute();
        MessageDlg( '���� ������� ��������!', mtInformation, [mbOK], 0 )
      end;
    end
    else if ( lvFiles.SelCount > 1 ) then
    begin
      Cmd := TMultiSignCommand.Create(
        TMultiViewer.Create( FileIconList ) as IMultiViewer ) as ICommand;
      GetSelectedFiles( ( Cmd as IMultiSignCommand ).Files );
      if not SetSign.Execute( Cmd ) then
      begin
        if Cmd.IsException() then
          MessageDlg( Cmd.ExceptionMsg, mtError, [mbOK], 0 );
      end
      else
        Refresh( ( Cmd as IMultiSignCommand ).Files, rtSign );
    end;
  finally
    Enabled := true;
  end;
end;

procedure TMainModule.aSetSignUpdate(Sender: TObject);
begin
  aSetSign.Enabled := Assigned( SetSign ) and ( lvFiles.SelCount > 0 ) and
      ( lvFiles.Items[lvFiles.ItemIndex].StateIndex = UNSIGN_IMAGE_INDEX );
end;

procedure TMainModule.aDelSignExecute(Sender: TObject);
var
  Files : TStringList;
begin
  if ( lvFiles.SelCount = 1 ) then
  begin
    if ( IDYES = Application.MessageBox(
        '�� ������������� ������ ������� �������?', '�������� �������',
        MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2 ) ) then
    begin
      FileModel.SingleDeleteSign(
        lvFiles.Items[lvFiles.ItemIndex].Caption );
      lvFiles.Items[lvFiles.ItemIndex].StateIndex := UNSIGN_IMAGE_INDEX;
      aCheckSign.Execute();
      MessageDlg( '������� ������� �������!', mtInformation, [mbOK], 0 );
    end;
  end
  else if ( lvFiles.SelCount > 1 ) then
  begin
    if ( IDYES = Application.MessageBox(
        '�� ������������� ������ ������� �������?', '�������� ��������',
        MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2 ) ) then
    begin
      Enabled := false;
      Files := NIL;
      try
        Files := TStringList.Create();
        GetSelectedFiles( Files );
        FileModel.MultiDeleteSign(
          TMultiViewer.Create( FileIconList ) as IMultiViewer, Files );
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
  aDelSign.Enabled := Assigned( SetSign ) and ( lvFiles.SelCount > 0 ) and
    ( lvFiles.Items[lvFiles.ItemIndex].StateIndex = SIGN_IMAGE_INDEX );
end;

procedure TMainModule.aChangeContainerExecute(Sender: TObject);
var
  Cmd : ICommand;
begin
  Cmd := TChangeContainerCommand.Create() as ICommand;
  if not SetSign.Execute( Cmd ) and Cmd.IsException() then
    MessageDlg( Cmd.ExceptionMsg, mtError, [mbOK], 0 );
end;

procedure TMainModule.aSettingExecute(Sender: TObject);
var
  Cmd : ICommand;
begin
  Cmd := TSettingCommand.Create() as ICommand;
  if not SetSign.Execute( Cmd ) and Cmd.IsException() then
    MessageDlg( Cmd.ExceptionMsg, mtError, [mbOK], 0 );
end;

procedure TMainModule.aAboutExecute(Sender: TObject);
begin
  About.ShowModal();
end;

procedure TMainModule.lvFilesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if ( Selected and ( lvFiles.SelCount = 1 ) ) then
    lvFiles.Tag := lvFiles.ItemIndex;
  if ( Item.StateIndex = lvFiles.Items[lvFiles.Tag].StateIndex ) then
    aCheckSign.Execute()
  else if Selected then
    Item.Selected := false;
end;

procedure TMainModule.lvFilesDblClick(Sender: TObject);
begin
  if ( lvFiles.ItemIndex <> -1 ) then
  begin
    if ( ShellExecute( Handle, 'open', PChar( FileModel.Directory +
        lvFiles.Items[lvFiles.ItemIndex].Caption ),
        NIL, NIL, SW_SHOWNORMAL ) < 32 ) then
      MessageDlg( '���� ���� �� ������� �������!', mtError, [mbOK], 0 );
  end;
end;

procedure TMainModule.lvFilesColumnClick(Sender: TObject; Column: TListColumn);
begin
  lvFiles.OnSelectItem := NIL;
  try
    lvFiles.ClearSelection();
  finally
    lvFiles.OnSelectItem := lvFilesSelectItem;
  end;
  if ( ArrowType = HDF_SORTDOWN ) then
    ArrowType := HDF_SORTUP
  else
    ArrowType := HDF_SORTDOWN;
  lvFiles.AlphaSort();
  SetColumnArrow( 0, ArrowType );
end;

procedure TMainModule.lvFilesCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  Compare := AnsiCompareText( Item1.Caption, Item2.Caption );
  if ( ArrowType = HDF_SORTDOWN ) then
    Compare := -Compare;
end;

procedure TMainModule.lvFilesResize(Sender: TObject);
begin
  SetColumnArrow( 0, ArrowType );
end;

procedure TMainModule.Refresh( Files: TStrings;
  RefreshType : TRefreshType = rtOpen );
var
  i : Integer;
  Item : TListItem;
begin
  lvFiles.Items.BeginUpdate();
  lvFiles.OnSelectItem := NIL;
  try
    if ( RefreshType = rtOpen ) then
    begin
      lvFiles.Clear;
      for i := 0 to Files.Count - 1 do
      begin
        Item := lvFiles.Items.Add();
        Item.Caption := Files.Strings[i];
        if Assigned( Files.Objects[i] ) then
          Item.StateIndex := SIGN_IMAGE_INDEX
        else
          Item.StateIndex := UNSIGN_IMAGE_INDEX;
        Item.ImageIndex := FileIconList.IndexOf( FileModel.Directory +
          Files.Strings[i] )
      end;
    end else
    begin
      Item := lvFiles.Selected;
      while ( Item <> NIl ) do
      begin
        if ( Files.IndexOf( Item.Caption ) <> - 1 ) then
        begin
          if ( RefreshType = rtSign ) then
            Item.StateIndex := SIGN_IMAGE_INDEX
          else
            Item.StateIndex := UNSIGN_IMAGE_INDEX;
        end else
          Item.Selected := false;
        Item := lvFiles.GetNextItem( Item, sdAll, [isSelected] );
      end;
      aCheckSign.Execute();
    end;
  finally
    lvFiles.OnSelectItem := lvFilesSelectItem;
    lvFiles.Items.EndUpdate();
  end;
end;

procedure TMainModule.GetSelectedFiles( Files : TStrings );
var
  Item : TListItem;
begin
  Item := lvFiles.Selected;
  while ( Item <> NIl ) do
  begin
    Files.Add( Item.Caption );
    Item := lvFiles.GetNextItem( Item, sdAll, [isSelected] );
  end;
end;

procedure TMainModule.SetColumnArrow( ColIndex : Integer; ArrowType : Integer );
var
 hHeader : HWND;
 hdItem : THDItem;
begin
  ZeroMemory( Pointer( @hdItem ), SizeOf( THDItem ) );
  hHeader := ListView_GetHeader( lvFiles.Handle );
  hdItem.Mask := HDI_FORMAT;
  Header_GetItem( hHeader, ColIndex, hdItem );
  hdItem.fmt := hdItem.fmt and not ( HDF_SORTDOWN or HDF_SORTUP );
  if ( ArrowType <> 0 ) then
    hdItem.fmt := hdItem.fmt or ArrowType;
  Header_SetItem( hHeader, ColIndex, hdItem );  
end;

end.

