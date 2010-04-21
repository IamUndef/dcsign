unit uFileModel;

interface

uses Classes, SysUtils, uIFileModel, uIModule, uICommands, uMainModule,
  uSignContext;

type

  TFileModel = class( TInterfacedObject, IFileModel, IModule )
    private
      const
        SIGN_EXT = '.sign';

    public
      const
        FILE_SIGN_FLAG = 1;
        
    private
      MainModule_ : TMainModule;
      Directory_ : String;

//      procedure CreateSign( FileName : String );

    public
      constructor Create( MainModule : TMainModule );
      destructor Destroy(); override;

      procedure Execute( Command  : ICommand );

      procedure Open( Directory : String );
      function Read( FileName : String ) : TBytes;
      function ReadSign( FileName : String ) : TSignContext;      
      procedure DeleteSign( FileName : String );

  end;

implementation

uses Dialogs;

constructor TFileModel.Create( MainModule: TMainModule );
begin
  MainModule_ := MainModule;
end;

destructor TFileModel.Destroy();
begin
//
end;

procedure TFileModel.Open( Directory : String );
var
  Index : Integer;
  SearchRec : TSearchRec;
  FilesInfo : TStringList;
begin
  FilesInfo := NIL;
  try
    if ( FindFirst( Directory + '\*.*', 0, SearchRec ) = 0 ) then
    begin
      try
        FilesInfo := TStringList.Create;
        while FindNext( SearchRec ) = 0 do
        begin
          Index := FilesInfo.Add( SearchRec.Name );
          if FileExists( Directory + '\' + SearchRec.Name + SIGN_EXT ) then
            FilesInfo.Objects[Index] := TObject( FILE_SIGN_FLAG );
        end;
      finally
        FindClose( SearchRec );
      end;
    end;
    MainModule_.Refresh( FilesInfo );
  finally
    if Assigned( FilesInfo ) then
      FilesInfo.Free;
  end;
  Directory_ := Directory;
end;

procedure TFileModel.Execute( Command: ICommand );
begin
{
  if Supports( Command, ITestCommand ) then
    ShowMessage( ( Command as ITestCommand ).GetMessage() )
  else
    MessageDlg( 'Команда не поддерживается!', mtError, [mbOK], 0 );
}
end;

function TFileModel.Read( FileName : String ) : TBytes;
begin
  // чтение содержимого файла
  Result := NIL;
end;

function TFileModel.ReadSign( FileName : String ) : TSignContext;
begin
  // чтение содержимого файла подписи
  Result := NIL;
end;

procedure TFileModel.DeleteSign( FileName : String );
begin
  // удаление файла подписи
end;

end.
