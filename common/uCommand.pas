unit uCommand;

interface

uses uICommands;

type

  TCommand = class( TInterfacedObject, ICommand )
    private
      ExceptionMsg_ : String;

    protected
      function GetExceptionMsg() : String;
      procedure SetExceptionMsg( Msg : String );

    public
      constructor Create();

      property ExceptionMsg : String read GetExceptionMsg write SetExceptionMsg;

  end;

implementation

constructor TCommand.Create();
begin
  inherited;
  ExceptionMsg_ := '';
end;

function TCommand.GetExceptionMsg() : String;
begin
  Result := ExceptionMsg_;
end;

procedure TCommand.SetExceptionMsg( Msg: String );
begin
  ExceptionMsg_ := Msg;
end;

end.
