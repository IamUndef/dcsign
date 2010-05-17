unit uSimpleCommand;

interface

uses uICommands, uCommand;

type

 TSimpleCommand = class( TCommand, ISimpleCommand )
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
  inherited Create();
  FileName_ := FileName;
end;

function TSimpleCommand.GetFileName() : String;
begin
  Result := FileName_;
end;

end.
