unit uAbout;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, pngimage;

type
  TAbout = class(TForm)
    pMain: TPanel;
    lbProgramLogo: TImage;
    lbProgramInfo: TLabel;
    bOk: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  About: TAbout;

implementation

{$R *.dfm}

end.

