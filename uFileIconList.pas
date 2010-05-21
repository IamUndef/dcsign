unit uFileIconList;

interface

uses Classes, ImgList;

type

  TFileIconList = class( TCustomImageList )
    private
      FileExt : TStrings;

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
  FileExt := TStringList.Create();
end;

destructor TFileIconList.Destroy();
begin
  if Assigned( FileExt ) then
    FileExt.Free();
  inherited;
end;

function TFileIconList.IndexOf( const FileName: String ) : Integer;
var
  Ext : String;
  Index : Integer;
  Icon : TIcon;
  FileInfo: SHFILEINFO;
begin
  Result := -1;
  Ext := LowerCase( ExtractFileExt( FileName ) );
  Index := FileExt.IndexOf( Ext );
  if ( Index <> -1 ) then
    Result := Integer( FileExt.Objects[Index] )
  else
  begin
    Icon := NIL;
    try
      if ( SHGetFileInfo( PChar( FileName ), 0, FileInfo,
          SizeOf( SHFILEINFO ), SHGFI_ICON or SHGFI_SMALLICON) <> 0 ) then
      begin
        Icon := TIcon.Create();
        Icon.Handle := FileInfo.hIcon;
        Result := AddIcon( Icon );
        FileExt.AddObject( Ext, TObject( Result ) );
      end;
    finally
      if Assigned( Icon ) then
        Icon.Free();
    end;
  end;
end;                     

end.
