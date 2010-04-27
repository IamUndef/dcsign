unit uFileModel;

interface

uses Classes, SysUtils, uIFileModel, uIModule, uICommands, uISignContext,
  uMainModule;

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
      Buffer : TBytes;

      procedure Execute( const Command : IReadCommand ); overload;
      procedure Execute( const Command : ICreateSignCommand ); overload;

      procedure CreateSign( const FileName : String; const Buffer : TBytes );

    public
      constructor Create( MainModule : TMainModule );
      destructor Destroy(); override;

      function Execute( const Command  : ICommand ) : Boolean; overload;

      procedure Open( const Directory : String );
      function Read( const FileName : String ) : TBytes;
      function ReadSign( const FileName : String ) : ISignContext;
      procedure DeleteSign( const FileName : String );

  end;

implementation

uses Dialogs, uSignContext;

constructor TFileModel.Create( MainModule: TMainModule );
begin
  MainModule_ := MainModule;
end;

destructor TFileModel.Destroy();
begin
  Buffer := NIL;
end;

function TFileModel.Execute( const Command: ICommand ) : Boolean;
begin
  Result := false;
  try
    if Supports( Command, IReadCommand ) then
      Execute( Command as IReadCommand )
    else
    if Supports( Command, ICreateSignCommand ) then
      Execute( Command as ICreateSignCommand )
    else
      raise Exception.Create( 'Команда не поддерживается!' );
    Result := true;
  except
    on E : Exception do
      MessageDlg( E.Message,  mtError, [mbOK], 0 );
  end;
end;

procedure TFileModel.Execute( const Command  : IReadCommand );
begin
  Command.Buffer := Read( Command.FileName );
end;

procedure TFileModel.Execute( const Command  : ICreateSignCommand );
begin
  CreateSign( Command.FileName, Command.SignContext.Buffer );
end;

procedure TFileModel.Open( const Directory : String );
var
  Index : Integer;
  SearchRec : TSearchRec;
  FilesInfo : TStringList;
begin
  FilesInfo := NIL;
  try
    if ( FindFirst( Directory + '*.*', 0, SearchRec ) = 0 ) then
    begin
      try
        FilesInfo := TStringList.Create;
        while FindNext( SearchRec ) = 0 do
          if ( ExtractFileExt( SearchRec.Name ) <> SIGN_EXT ) then
          begin
            Index := FilesInfo.Add( SearchRec.Name );
            if FileExists( Directory + SearchRec.Name + SIGN_EXT ) then
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

function TFileModel.Read( const FileName : String ) : TBytes;
var
  FileStream : TFileStream;
begin
  Result := NIL;
  FileStream := NIL;
  Buffer := NIL;
  try
    FileStream := TFileStream.Create( Directory_ + FileName, fmOpenRead  );
    if ( FileStream.Size = 0 ) then
      raise Exception.Create( 'Файл "' + Directory_ + FileName
        + '" пустой!' );
    try
      SetLength( Buffer, FileStream.Size );
      FileStream.ReadBuffer( Buffer[0], FileStream.Size );
    except
      Buffer := NIL;
      raise;
    end;
  finally
    if Assigned( FileStream ) then
      FileStream.Free;
  end;
  Result := Buffer;
end;

function TFileModel.ReadSign( const FileName : String ) : ISignContext;
begin
  // чтение содержимого файла подписи
  Result := NIL;
end;

procedure TFileModel.DeleteSign( const FileName : String );
begin
  // удаление файла подписи
end;

procedure TFileModel.CreateSign( const FileName : String; const Buffer : TBytes );
var
  FileStream : TFileStream;
begin
  FileStream := NIL;
  if not Assigned( Buffer ) then
    raise Exception.Create( 'Данные для записи файла "' + FileName + SIGN_EXT +
      '" не удалось получить!' );
  try
    FileStream := TFileStream.Create( Directory_ + FileName + SIGN_EXT,
      fmCreate );
    FileStream.WriteBuffer( Buffer[0], Length( Buffer ) );
    MainModule_.Refresh( FileName );
  finally
    if Assigned( FileStream ) then
      FileStream.Free;
  end;
end;

end.
