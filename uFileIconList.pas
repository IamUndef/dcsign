unit uFileIconList;

interface

uses Classes, ImgList;

type

  TFileIconList = class( TCustomImageList )
    private
      LinkedIcon : TStrings;

    public
      constructor Create( Owner: TComponent ); override;
      destructor Destroy; override;

      function IndexOf( const FileName : String ) : Integer;

  end;

implementation

uses SysUtils, Graphics, ShellApi;

constructor TFileIconList.Create( Owner: TComponent );
begin
  inherited Create( Owner );
  LinkedIcon := TStringList.Create();
end;

destructor TFileIconList.Destroy();
begin
  if Assigned( LinkedIcon ) then
    LinkedIcon.Free();
  inherited;
end;

function TFileIconList.IndexOf( const FileName: String ) : Integer;
var
  Ext : String;
  Index : Integer;

function AddLinkedIcon( FileId : String ) : Integer;
var
  Icon : TIcon;
  FileInfo: SHFILEINFO;
begin
  Result := -1;
  Icon := NIL;
  try
    if ( SHGetFileInfo( PChar( FileName ), 0, FileInfo,
        SizeOf( SHFILEINFO ), SHGFI_ICON or SHGFI_SMALLICON) <> 0 ) then
    begin
      Icon := TIcon.Create();
      Icon.Handle := FileInfo.hIcon;
      Result := AddIcon( Icon );
      LinkedIcon.AddObject( FileId, TObject( Result ) )
    end;
  finally
    if Assigned( Icon ) then
      Icon.Free();
  end;
end;

begin
  Result := -1;
  Ext := LowerCase( ExtractFileExt( FileName ) );
  if ( ( Ext = '.exe' ) or ( Ext = '.dll' ) ) then
  begin
    Index := LinkedIcon.IndexOf( ExtractFileName( FileName ) );
    if ( Index <> -1 ) then
    begin
      if ( ExtractFilePath( FileName ) = '' ) then
        Result := Integer( LinkedIcon.Objects[Index] )
      else
      begin
        LinkedIcon.Delete( Index );
        Result := AddLinkedIcon( ExtractFileName( FileName ) );
      end
    end
    else if ( ExtractFilePath( FileName ) <> '' ) then
      Result := AddLinkedIcon( ExtractFileName( FileName ) );
  end
  else
  begin
    Index := LinkedIcon.IndexOf( Ext );
    if ( Index <> -1 ) then
      Result := Integer( LinkedIcon.Objects[Index] )
    else if ( ExtractFilePath( FileName ) <> '' ) then
      Result := AddLinkedIcon( Ext );
  end;
end;

end.
