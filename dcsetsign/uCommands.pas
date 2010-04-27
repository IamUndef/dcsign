unit uCommands;

interface

uses SysUtils, uICommands, uISignContext, uSimpleCommand;

type

  TReadCommand = class( TSimpleCommand, IReadCommand )
    private
      Buffer_ : TBytes;

    protected
      function GetBuffer() : TBytes;
      procedure SetBuffer( const Buffer : TBytes );

    public
      property Buffer : TBytes read GetBuffer write SetBuffer;

  end;

  TCreateSignCommand = class( TSimpleCommand, ICreateSignCommand )
    private
      SignContext_ : ISignContext;

    protected
      function GetSignContext() : ISignContext;

    public
      constructor Create( const FileName : String );

      property SignContext : ISignContext read GetSignContext;
  end;

implementation

uses uSignContext;

function TReadCommand.GetBuffer() : TBytes;
begin
  Result := Buffer_;
end;

procedure TReadCommand.SetBuffer( const Buffer : TBytes );
begin
  Buffer_ := Buffer;
end;

constructor TCreateSignCommand.Create( const FileName : String );
begin
  inherited Create( FileName );
  SignContext_ := TSignContext.Create();
end;

function TCreateSignCommand.GetSignContext() : ISignContext;
begin
  Result := SignContext_;
end;

end.
