unit uFileModel;

interface

uses Classes, SysUtils, uIFileModel, uIModule, uICommands, uISignContext,
  uMainModule;

type

  TFileModel = class( TInterfacedObject, IFileModel, IModule )
    private
      const
        SIGN_EXT = '.sign';
        FILE_SIGN_FLAG = 1;
        
    private
      Directory_ : String;

      procedure Execute( const Command : IReadCommand ); overload;
      procedure Execute( const Command : ICreateSignCommand ); overload;

      procedure CreateSign( const FileName : String; const Buffer : TBytes );

    public
      function Execute( const Command  : ICommand ) : Boolean; overload;

      procedure Open( const Directory : String; Files : TStrings );
      function Read( const FileName : String ) : TBytes;
      function ReadSign( const FileName : String ) : ISignContext;
      procedure DeleteSign( const FileName : String );
      //MultiDeleteSign

  end;

implementation

uses Windows, Dialogs, uSignContext;

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
      Command.ExceptionMsg := E.Message;
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

procedure TFileModel.Open( const Directory : String; Files : TStrings );
var
  Index : Integer;
  SearchRec : TSearchRec;
  OpenedDir : String;
begin
  OpenedDir := Directory;
  if ( OpenedDir[ Length( OpenedDir ) ] <> '\' ) then
    OpenedDir := OpenedDir + '\';
  if ( FindFirst( OpenedDir + '*.*', 0, SearchRec ) = 0 ) then
  begin
    try
      repeat
        if ( ExtractFileExt( SearchRec.Name ) <> SIGN_EXT ) then
        begin
          Index := Files.Add( SearchRec.Name );
          if FileExists( OpenedDir + SearchRec.Name + SIGN_EXT ) then
            Files.Objects[Index] := TObject( FILE_SIGN_FLAG );
        end;
      until FindNext( SearchRec ) <> 0;
    finally
      SysUtils.FindClose( SearchRec );
    end;
  end;
  Directory_ := OpenedDir;
end;

function TFileModel.Read( const FileName : String ) : TBytes;
var
  FileStream : TFileStream;
begin
  Result := NIL;
  FileStream := NIL;
  try
    FileStream := TFileStream.Create( Directory_ + FileName, fmOpenRead );
    if ( FileStream.Size = 0 ) then
      raise Exception.Create( 'Файл "' + Directory_ + FileName
        + '" пустой!' );
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

function TFileModel.ReadSign( const FileName : String ) : ISignContext;
var
  FileStream : TFileStream;
  Buffer : TBytes;
begin
  Result := NIL;
  FileStream := NIL;
  try
    FileStream := TFileStream.Create( Directory_ + FileName + SIGN_EXT,
      fmOpenRead );
    if ( FileStream.Size = 0 ) then
      raise Exception.Create( 'Файл "' + Directory_ + FileName + SIGN_EXT
        + '" пустой!' );
    try
      Result := TSignContext.Create;
      SetLength( Buffer, FileStream.Size );
      FileStream.ReadBuffer( Buffer[0], FileStream.Size );
      Result.Buffer := Buffer;
      if not Result.IsValid() then
        raise Exception.Create( 'В файле "' + Directory_ + FileName + SIGN_EXT
          + '" некорректные данные!' );  
    except
      Result := NIL;
      raise;
    end;
  finally
    if Assigned( FileStream ) then
      FileStream.Free;
  end;
end;

procedure TFileModel.DeleteSign( const FileName : String );
begin
  FileSetAttr( Directory_ + FileName + SIGN_EXT, 0 );
  if not SysUtils.DeleteFile( Directory_ + FileName + SIGN_EXT ) then
    raise Exception.Create( 'Не удалось удалить файл "' + Directory_ +
      FileName + SIGN_EXT + '"!' );
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
    FileSetAttr( Directory_ + FileName + SIGN_EXT, {faHidden or} faReadOnly );
  finally
    if Assigned( FileStream ) then
      FileStream.Free;
  end;
end;

end.
