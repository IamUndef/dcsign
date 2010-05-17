unit uMultiViewer;

interface

uses uIMultiViewer, uICheckSign, uMultiViewerForm;

type

  TMultiViewer = class( TInterfacedObject, IMultiViewer )
    private
      Form : TMultiViewerForm;

    public
      constructor Create( const CheckSign : ICheckSign = NIL );
      destructor Destroy(); override;

      procedure Show( Caption : String );
      procedure Hide( Msg : String );
      procedure AddFile( IsOk : Boolean; FileName : String; Result : String );

  end;

implementation

constructor TMultiViewer.Create( const CheckSign: ICheckSign = NIL );
begin
  inherited Create();
  Form := TMultiViewerForm.Create( CheckSign );
end;

destructor TMultiViewer.Destroy();
begin
  if Assigned( Form ) then
    Form.Free();
  inherited;
end;

procedure TMultiViewer.Show( Caption : String );
begin
  Form.Show( Caption );
end;

procedure TMultiViewer.Hide( Msg : String );
begin
  Form.Hide( Msg );
end;

procedure TMultiViewer.AddFile( IsOk : Boolean; FileName : String;
  Result : String );
begin
  Form.AddFile( IsOk, FileName, Result );
end;

end.
