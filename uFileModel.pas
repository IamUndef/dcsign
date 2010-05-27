unit uFileModel;

interface

uses
  Classes, SysUtils, uIFileModel, uIModule, uICommands, uIMultiViewer,
  uISignContext;

type

  TFileModel = class( TInterfacedObject, IFileModel, IModule )
    private
      const
        SIGN_EXT = '.sign';
        FILE_SIGN_FLAG = 1;

        MULTI_DELSIGN_RESULT_CAPTION = 'Удаление подписей файлов';
        MULTI_DELSIGN_RESULT_SUCC = 'Удалена';
        MULTI_DELSIGN_RESULT_MSG = 'Удаление подписей файлов завершено!';

        MULTI_COPY_RESULT_CAPTION = 'Копирование подписанных файлов';
        MULTI_COPY_RESULT_SUCC = 'Скопирован';
        MULTI_COPY_RESULT_MSG = 'Копирование подписанных файлов завершено!';

    private
      Directory_ : String;

      procedure Execute( const Command : IReadCommand ); overload;
      procedure Execute( const Command : ICreateSignCommand ); overload;

      procedure CreateSign( const FileName : String; const Buffer : TBytes );

    protected
      function GetDirectory() : String;

    public
      function Execute( const Command  : ICommand ) : Boolean; overload;

      procedure Open( const Directory : String; Files : TStrings );
      function Read( const FileName : String ) : TBytes;
      function ReadSign( const FileName : String ) : ISignContext;
      procedure SingleDeleteSign( const FileName : String );
      procedure MultiDeleteSign( const Viewer : IMultiViewer;
        Files : TStrings );
      procedure SingleCopy( const Dir : String; const FileName : String );
      procedure MultiCopy( const Viewer : IMultiViewer; const Dir : String;
        Files : TStrings );

      property Directory : String read GetDirectory;

  end;

implementation

uses Windows, uSignContext;

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
      raise Exception.Create( Format( 'Файл "%s" пустой!',
        [Directory_ + FileName] ) );
    try
      SetLength( Result, FileStream.Size );
      FileStream.ReadBuffer( Result[0], FileStream.Size );
    except
      Result := NIL;
      raise;
    end;
  finally
    if Assigned( FileStream ) then
      FileStream.Free();
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
      raise Exception.Create( Format( 'Файл "%s" пустой!',
        [Directory_ + FileName + SIGN_EXT] ) );
    try
      Result := TSignContext.Create;
      SetLength( Buffer, FileStream.Size );
      FileStream.ReadBuffer( Buffer[0], FileStream.Size );
      Result.Buffer := Buffer;
      if not Result.IsValid() then
        raise Exception.Create( Format( 'В файле "%s" некорректные данные!',
          [Directory_ + FileName + SIGN_EXT] ) );
    except
      Result := NIL;
      raise;
    end;
  finally
    if Assigned( FileStream ) then
      FileStream.Free();
  end;
end;

procedure TFileModel.SingleDeleteSign( const FileName : String );
begin
  FileSetAttr( Directory_ + FileName + SIGN_EXT, 0 );
  if not SysUtils.DeleteFile( Directory_ + FileName + SIGN_EXT ) then
    raise Exception.Create( Format( 'Не удалось удалить файл "%s"!',
      [Directory_ + FileName + SIGN_EXT] ) );
end;

procedure TFileModel.MultiDeleteSign( const Viewer : IMultiViewer;
  Files : TStrings );
var
  i : Integer;
begin
  Viewer.Show( MULTI_DELSIGN_RESULT_CAPTION );
  try
    i := 0;
    while ( i < Files.Count ) do
    begin
      try
        SingleDeleteSign( Files[i] );
        Viewer.AddFile( true, Files[i], MULTI_DELSIGN_RESULT_SUCC );
      except
        on E : Exception do
        begin
          Viewer.AddFile( false, Files[i], E.Message );
          Files.Delete( i );
          Dec( i );
        end;
      end;
      Inc( i );
    end;
  finally
    Viewer.Hide( MULTI_DELSIGN_RESULT_MSG );
  end;
end;

procedure TFileModel.SingleCopy( const Dir : String; const FileName : String );
var
  DestDir : String;
begin
  DestDir := Dir;
  if ( DestDir[ Length( DestDir ) ] <> '\' ) then
    DestDir := DestDir + '\';
  if not CopyFile( PChar( Directory_ + FileName + SIGN_EXT ),
      PChar( DestDir + FileName + SIGN_EXT ), true ) then
    raise Exception.Create( Format( 'Не удалось скопировать файл "%s": %s!',
      [Directory_ + FileName + SIGN_EXT, SysErrorMessage( GetLastError() )] ) );
  if not CopyFile( PChar( Directory_ + FileName ), PChar( DestDir + FileName ),
      true ) then
    raise Exception.Create( Format( 'Не удалось скопировать файл "%s", %s!',
      [Directory_ + FileName, SysErrorMessage( GetLastError() )] ) );
end;

procedure TFileModel.MultiCopy( const Viewer : IMultiViewer; const Dir : String;
  Files : TStrings );
var
  DestDir : String;
  i : Integer;
begin
  DestDir := Dir;
  if ( DestDir[ Length( DestDir ) ] <> '\' ) then
    DestDir := DestDir + '\';
  Viewer.Show( MULTI_COPY_RESULT_CAPTION );
  try
    for i := 0 to Files.Count - 1 do
    begin
      try
        SingleCopy( DestDir, Files[i] );
        Viewer.AddFile( true, Files[i], MULTI_COPY_RESULT_SUCC );
      except
        on E : Exception do
          Viewer.AddFile( false, Files[i], E.Message );
      end;
    end;
  finally           
    Viewer.Hide( MULTI_COPY_RESULT_MSG );
  end;
end;

procedure TFileModel.CreateSign( const FileName : String; const Buffer : TBytes );
var
  FileStream : TFileStream;
begin
  FileStream := NIL;
  if not Assigned( Buffer ) then
    raise Exception.Create( Format(
      'Не удалось получить данные для создания файла "%s"!',
      [FileName + SIGN_EXT] ) );
  try
    FileStream := TFileStream.Create( Directory_ + FileName + SIGN_EXT,
      fmCreate );
    FileStream.WriteBuffer( Buffer[0], Length( Buffer ) );
    FileSetAttr( Directory_ + FileName + SIGN_EXT, {faHidden or} faReadOnly );
  finally
    if Assigned( FileStream ) then
      FileStream.Free();
  end;
end;

function TFileModel.GetDirectory() : String;
begin
  Result := Directory_;
end;

end.
