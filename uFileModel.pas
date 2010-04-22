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

      function Execute( Command  : ICommand ) : Boolean; overload;
      function Execute( Command  : IReadCommand ) : Boolean; overload;

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

function TFileModel.Execute( Command: ICommand ) : Boolean;
begin
  Result := false;
  if Supports( Command, IReadCommand ) then
    Result := Execute( Command as IReadCommand )
  else
    MessageDlg( 'Команда не поддерживается!', mtError, [mbOK], 0 );
end;

function TFileModel.Execute( Command  : IReadCommand ) : Boolean;
begin
  Result := false;
  try
    Command.Data := Read( Command.Argument );
    Result := true;
  except
    on E : Exception do
      MessageDlg( E.Message,  mtError, [mbOK], 0 );
  end;
end;

function TFileModel.Read( FileName : String ) : TBytes;
var
  FileStream : TFileStream;
begin
  Result := NIL;
  FileStream := NIL;
  try
    FileStream := TFileStream.Create( Directory_ + '\' + FileName, fmOpenRead  );
    try
      SetLength( Result, FileStream.Size );
      FileStream.ReadBuffer( Result[0], FileStream.Size );
    except
      Result := NIL;
      raise;
    end;
  finally
    if Assigned( FileStream ) then
      FileStream.Free;
  end;
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
