unit uDM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs;

type
  TDataModule1 = class(TDataModule)
    FDConnection: TFDConnection;
    FDQGetToken: TFDQuery;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetToken: string;
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}
{ TDataModule1 }

function TDataModule1.GetToken: string;
begin
  FDQGetToken.Close;

  FDQGetToken.Open;
  try
    if (FDQGetToken.RecordCount > 0) then
    begin
      FDQGetToken.First;
      while not FDQGetToken.Eof do
      begin
        Result := FDQGetToken.FieldByName('Token').AsString;

      end;
    end;
  finally
    FDQGetToken.Close;
  end;
end;

end.
