program PrjTicketMaster;

uses
  Vcl.Forms,
  uMainFrm in 'uMainFrm.pas' {Form1},
  uDM in 'DM\uDM.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
