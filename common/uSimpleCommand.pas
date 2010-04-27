unit uSimpleCommand;

interface

uses uICommands;

type

 TSimpleCommand = class( TInterfacedObject, ICommand, ISimpleCommand )
    private
      FileName_ : String;

    protected
      function GetFileName() : String;

    public
      constructor Create( const FileName : String );

      property FileName : String read GetFileName;

  end;

implementation

constructor TSimpleCommand.Create( const FileName : String );
begin
  FileName_ := FileName;
end;

function TSimpleCommand.GetFileName() : String;
begin
  Result := FileName_;
end;

end.
