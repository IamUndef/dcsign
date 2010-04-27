unit uMainModule;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ComCtrls, ImgList, Menus, ActnList, uIFileModel,
  uIModule;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aSetSignExecute(Sender: TObject);
    
  private
    const
      SETSIGN_DLL = 'dcsetsign.dll';
      
  private
    FileModel : IFileModel;
//  Объект класса проверки подпичи
    SetSign : IModule;

  public
    procedure Refresh( FilesInfo : TStringList ); overload;
    procedure Refresh( FileName : String ); overload;

  end;

var
  MainModule: TMainModule;

implementation

{$R *.dfm}

uses uFileModel, uICommands, uCommands;

procedure TMainModule.FormCreate(Sender: TObject);

type
  TGetInstance = function ( const FileModel : IModule ) : IModule;
  
var
  SetSignDLL : THandle;
  GetInstance : TGetInstance;

begin
  FileModel := TFileModel.Create( Self );
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

procedure TMainModule.aSetSignExecute(Sender: TObject);
begin
  if ( Assigned( SetSign ) and ( lvFiles.ItemIndex <> -1 ) ) then
    SetSign.Execute( TSignCommand.Create(
      lvFiles.Items[lvFiles.ItemIndex].SubItems[0] ) as ICommand );
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
    lvFiles.Invalidate;
  end;
end;

end.
