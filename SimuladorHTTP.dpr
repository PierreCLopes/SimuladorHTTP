program SimuladorHTTP;

uses
  Vcl.Forms,
  uFormSimuladorHTTP in 'uFormSimuladorHTTP.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
