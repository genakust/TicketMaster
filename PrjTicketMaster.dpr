program PrjTicketMaster;

uses
  Vcl.Forms,
  uMainFrm in 'uMainFrm.pas' {frmTicketmaster},
  uDM in 'DM\uDM.pas' {DataModule1: TDataModule},
  uAppData in 'AppData\uAppData.pas',
  uTiketmasterApi in 'AppData\uTiketmasterApi.pas',
  uController in 'Controller\uController.pas',
  uResourceStrings in 'AppData\uResourceStrings.pas',
  uModelList in 'Model\uModelList.pas',
  uModel in 'Model\uModel.pas',
  uJsonUtills in 'JSON\uJsonUtills.pas',
  uListViewCommand in 'ListView\uListViewCommand.pas',
  uListViewSort in 'ListView\uListViewSort.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTicketmaster, frmTicketmaster);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
