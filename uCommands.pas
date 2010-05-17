unit uCommands;

interface

uses Classes, uICommands, uIMultiViewer, uCommand, uSimpleCommand;

type

  TSignCommand = class( TSimpleCommand, ISignCommand )
  end;

  TMultiSignCommand = class( TCommand, IMultiSignCommand )
    private
      Viewer_ : IMultiViewer;
      Files_ : TStrings;

    protected
      function GetViewer() : IMultiViewer;
      function GetFiles() : TStrings;

    public
      constructor Create();
      destructor Destroy(); override;

      property Viewer : IMultiViewer read GetViewer;
      property Files : TStrings read GetFiles;

  end;

  TSettingCommand = class( TCommand, ISettingCommand )
  end;

implementation

uses uMultiViewer;

constructor TMultiSignCommand.Create();
begin
  inherited;
  Viewer_ := TMultiViewer.Create();
  Files_ := NIL;
end;

destructor TMultiSignCommand.Destroy;
begin
  if Assigned( Files_ ) then
    Files_.Free();
end;

function TMultiSignCommand.GetViewer() : IMultiViewer;
begin
  Result := Viewer_;
end;

function TMultiSignCommand.GetFiles() : TStrings;
begin
  if not Assigned( Files_ ) then
    Files_ := TStringList.Create;
  Result := Files_;
end;

end.
