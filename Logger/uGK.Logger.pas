unit uGK.Logger;

interface

uses
  {Delphi}
  Winapi.Windows, System.Classes;

type
  TLogger = class
  private
    FOnLoggingEvent: TGetStrProc;
    class var FUniqueInstance: TLogger;
    constructor Create;
  public
    destructor Destroy; override;
    class function GetInstance: TLogger; static;
    ///<summary> the error message to log into the debug window </summary>
    procedure Log(const aMessage: string);
    ///<summary> the error message to log into the file <c>events.log</c>
    /// in the app directory </summary>
    procedure LogInFile(const aMessage: string);
    Property OnLoggingEvent : TGetStrProc read  FOnLoggingEvent write FOnLoggingEvent;
  end;

var
  MonitorObj: TObject;
  logger: TLogger;

implementation

uses
  System.SysUtils;

{ TLogger }

constructor TLogger.Create;
begin
  inherited;
end;

destructor TLogger.Destroy;
begin

  inherited;
end;

class function TLogger.GetInstance: TLogger;
begin
  TMonitor.Enter(MonitorObj);
  try
    if FUniqueInstance = nil then
    begin
      FUniqueInstance := TLogger.Create;
    end;
  finally
    TMonitor.Exit(MonitorObj);
  end;
  Result := FUniqueInstance;
end;

procedure TLogger.Log(const aMessage: string);
begin
  OutputDebugString(PChar(aMessage));
  if Assigned(FOnLoggingEvent) then
    FOnLoggingEvent(aMessage);
end;

procedure TLogger.LogInFile(const aMessage: string);
var
  logLine: string;
  fileName: string;
  myFile : TextFile;
begin
  fileName := ExtractFileDir(ParamStr(0)) + '\events.log';
  logLine := DateTimeToStr(Now) + ' : ' + aMessage;
  AssignFile(myFile, fileName);
  if not FileExists(fileName) then
    Rewrite(myFile)
  else
    Append(myFile);
  try
    WriteLn(myFile, logLine);
  finally
    CloseFile(myFile);
  end;
  if Assigned(FOnLoggingEvent) then
    FOnLoggingEvent(aMessage);
end;

initialization
  MonitorObj := TObject.Create;
  logger:= TLogger.GetInstance;

finalization
  logger.Free;
  MonitorObj.free;

end.
