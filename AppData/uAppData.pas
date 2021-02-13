unit uAppData;

interface

uses
  FireDAC.Comp.Client, System.SysUtils;

type

  /// <summary>
  TAppData = class
  private
    class var FAppData: TAppData;
    function GetToken: string;
  public
    destructor Destroy; override;
    class function GetInstance: TAppData;
    property Token: string read GetToken;
  end;

var
  MonitorObj: TObject;

implementation

uses
  uDM;

{ TAppData }


destructor TAppData.Destroy;
begin

  inherited;
end;

class function TAppData.GetInstance: TAppData;
begin
  TMonitor.Enter(MonitorObj);
  try
    if not Assigned(FAppData) then
      FAppData := TAppData.Create;
    Result := FAppData;
  finally
    TMonitor.Exit(MonitorObj);
  end;
end;

function TAppData.GetToken: string;
begin
  Result :=  DataModule1.GetToken;
end;

initialization

MonitorObj := TObject.Create;

finalization

MonitorObj.DisposeOf;

end.
