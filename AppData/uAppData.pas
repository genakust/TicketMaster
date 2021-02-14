unit uAppData;

interface

uses
  FireDAC.Comp.Client, System.SysUtils;

type
  IAppData = interface
    ['{6D571E40-F272-43F8-A7C4-E899CBE82371}']
    function GetToken: string;
    property Token: string read GetToken;
  end;

  /// <summary>Token holder as Singleton.
  /// </summary>
  /// <remarks>It is a private key.
  /// </remarks>
  TAppData = class sealed(TinterfacedObject, IAppData)
  private
    class var FAppData: TAppData;
    function GetToken: string;
  public
    destructor Destroy; override;
    class function GetInstance: IAppData;
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

class function TAppData.GetInstance: IAppData;
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
