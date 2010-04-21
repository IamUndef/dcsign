unit uMainModule;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, uIFileModel, uIModule;

type
  TMainModule = class(TForm)
    gbMain: TGroupBox;
    sgFiles: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    
  private
    const
      SETSIGN_DLL = 'dcsetsign.dll';
      
  private
    FileModel : IFileModel;
//  Объект класса проверки подпичи
    SetSign : IModule;

  public
    procedure Refresh( FilesInfo : TStringList );

  end;

var
  MainModule: TMainModule;

implementation

{$R *.dfm}

uses uFileModel, uICommands, uCommands;

procedure TMainModule.FormCreate(Sender: TObject);

type
  TGetInstance = function ( FileModel : IModule ) : IModule;
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

procedure TMainModule.Refresh( FilesInfo : TStringList );
var
  i : Integer;
begin
  sgFiles.RowCount := FilesInfo.Count;
  for i := 0 to FilesInfo.Count - 1 do
  begin
    sgFiles.Cells[0, i] := IntToStr( Integer( FilesInfo.Objects[i] ) );
    sgFiles.Cells[1, i] := FilesInfo.Strings[i];
  end;
end;

end.
