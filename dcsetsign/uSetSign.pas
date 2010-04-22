unit uSetSign;

interface

uses SysUtils, uIModule, uICommands;

type
  TSetSign = class( TInterfacedObject, IModule )
    private
      Model : IModule;
      CSPName : String;
      Container : String;
      HashId : Integer;

    public
        constructor Create( FileModel : IModule );

        function Execute( Command : ICommand ) : Boolean; overload;
        function Execute( Command : ISignCommand ) : Boolean; overload;

        function Sign( Buffer : TBytes ) : Boolean;

  end;

  function GetInstance( FileModel : IModule ) : IModule; export;

implementation

uses Dialogs, uCommands;

function GetInstance( FileModel : IModule ) : IModule; export;
begin
  Result := TSetSign.Create( FileModel );
end;

constructor TSetSign.Create( FileModel: IModule );
begin
  Model := FileModel;
end;

function TSetSign.Execute( Command : ICommand ) : Boolean;
begin
  Result := false;
  if Supports( Command, ISignCommand ) then
    Result := Execute( Command as ISignCommand )
  else
    MessageDlg( 'Команда не поддерживается!', mtError, [mbOK], 0 );
end;

function TSetSign.Execute( Command : ISignCommand ) : Boolean;
var
  ReadCmd : IReadCommand;
begin
  Result := false;
  ReadCmd := TReadCommand.Create( Command.Argument );
  if ( Model.Execute( ReadCmd as ICommand ) and Assigned( ReadCmd.Data ) ) then
    Result := Sign( ReadCmd.Data );
end;

function TSetSign.Sign( Buffer: TBytes ) : Boolean;
begin
  Result := false;
  try
    
    Result := true;
  except
    on E : Exception do
      MessageDlg( E.Message,  mtError, [mbOK], 0 );
  end;
end;

end.
