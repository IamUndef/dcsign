unit uSetSign;

interface

uses uIModule, uICommands;

type
  TSetSign = class( TInterfacedObject, IModule )
    private
      Model : IModule;

    public
        constructor Create( FileModel : IModule );

        procedure Execute( Command : ICommand );

  end;

  function GetInstance( FileModel : IModule ) : IModule; export;

implementation

uses SysUtils, Dialogs, uCommands;

function GetInstance( FileModel : IModule ) : IModule; export;
begin
  Result := TSetSign.Create( FileModel );
end;

constructor TSetSign.Create( FileModel: IModule );
begin
  Model := FileModel;
end;

procedure TSetSign.Execute( Command : ICommand );
begin
{
  if Supports( Command, ITestCommand ) then
  begin
    ShowMessage( ( Command as ITestCommand ).GetMessage() );
    Model.Execute( TTestCommand.Create( 'Test Message 2!' ) as ICommand )
  end
  else
    MessageDlg( 'Команда не поддерживается!', mtError, [mbOK], 0 );
  }
end;

end.
