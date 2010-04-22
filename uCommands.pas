unit uCommands;

interface

uses SysUtils, uICommands;

type

  TSimpleCommand = class( TInterfacedObject, ICommand, ISimpleCommand )
    private
      Arg : String;

    public
      constructor Create( Argument : String );

      function GetArgument() : String;

      property Argument : String read GetArgument;

  end;

  TSignCommand = class( TSimpleCommand, ISignCommand )
  end;

  TReadCommand = class( TSimpleCommand, IReadCommand )
    private
      Data_ : TBytes;

    public
      procedure SetData( Data : TBytes );
      function GetData() : TBytes;

      property Data : TBytes read GetData write SetData;

  end;

implementation

constructor TSimpleCommand.Create( Argument: String );
begin
  Arg := Argument;
end;

function TSimpleCommand.GetArgument() : String;
begin
  Result := Arg;
end;

procedure TReadCommand.SetData( Data: TBytes );
begin
  Data_ := Data;
end;

function TReadCommand.GetData() : TBytes;
begin
  Result := Data_;
end;

end.
