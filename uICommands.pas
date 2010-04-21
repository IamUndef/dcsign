unit uICommands;

interface

type

  ICommand = interface( IInterface )
    ['{44159665-1DCC-438D-8059-ED8EA035C467}']
  end;

  ITestCommand = interface( IInterface )
    ['{681CA845-B4D0-48D9-9CA8-D03F6BF16A78}']

    function GetMessage() : String;

  end;  

implementation

end.
