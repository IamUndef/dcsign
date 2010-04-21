unit uSignContext;

interface

uses SysUtils;

type

  TSignContext = class( TObject )
    private
      CSPName_ : String;
      HashId_ : Integer;
      FileName_ : String;
      DateTime_ : TDateTime;
      Certs_ : array of TBytes;

    public
      constructor Create( Buffer : TBytes ); 

  end;

implementation

constructor TSignContext.Create( Buffer: TBytes );
var
  Pos : Integer;
begin
  Pos := 0;
{
  ParseCSPName( Buffer, Pos );
  ParseHashId( Buffer, Pos );
  ParseFileName( Buffer, Pos );
  ParseDateTime( Buffer, Pos );
  ParseCerts( Buffer, Pos );
}
end;

end.
