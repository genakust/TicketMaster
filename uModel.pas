unit uModel;

interface

type
  IModel = interface
    ['{ECAC6E3F-6A02-4A52-8B55-C47A2DDE960B}']
    function GetEventName: string;
    function GetEventUrl: string;
    function GetLocalDate: string;
    function GetLocalTime: string;
    procedure SetEventName(const Value: string);
    procedure SetEventUrl(const Value: string);
    procedure SetLocalDate(const Value: string);
    procedure SetLocalTime(const Value: string);
    property EventName: string read GetEventName write SetEventName;
    property EventUrl: string read GetEventUrl write SetEventUrl;
    property LocalTime: string read GetLocalTime write SetLocalTime;
    property LocalDate: string read GetLocalDate write SetLocalDate;
  end;

  TModel = class(TInterfacedObject, IModel)
  private
    FEventName, FEventUrl, FLocalTime, FLocalDate: string;
    (* Properties *)
    function GetEventName: string;
    function GetEventUrl: string;
    function GetLocalDate: string;
    function GetLocalTime: string;
    procedure SetEventName(const Value: string);
    procedure SetEventUrl(const Value: string);
    procedure SetLocalDate(const Value: string);
    procedure SetLocalTime(const Value: string);
  public
    constructor Create; overload;
    constructor Create(const aEventName, aEventUrl, aLocalTime,
      aLocalDate: string); overload;
    destructor Destroy; override;
    (* Properties *)
    property EventName: string read GetEventName write SetEventName;
    property EventUrl: string read GetEventUrl write SetEventUrl;
    property LocalTime: string read GetLocalTime write SetLocalTime;
    property LocalDate: string read GetLocalDate write SetLocalDate;
  end;

implementation

uses
  System.SysUtils;

{ TModel }

{$REGION '< TModel >'}
constructor TModel.Create(const aEventName, aEventUrl, aLocalTime,
  aLocalDate: string);
begin
  FEventName := aEventName;
  FEventUrl := aEventUrl;
  FLocalTime := aLocalTime;
  FLocalDate := aLocalDate;
end;

constructor TModel.Create;
begin
  inherited;
  FEventName := EmptyStr;
  FEventUrl := EmptyStr;
  FLocalTime := EmptyStr;
  FLocalDate := EmptyStr;
end;

destructor TModel.Destroy;
begin

  inherited;
end;

function TModel.GetEventName: string;
begin
  Result := FEventName;
end;

function TModel.GetEventUrl: string;
begin
  Result := FEventUrl;
end;

function TModel.GetLocalDate: string;
begin
  Result := FLocalDate;
end;

function TModel.GetLocalTime: string;
begin
  Result := FLocalTime;
end;

procedure TModel.SetEventName(const Value: string);
begin
  FEventName := Value;
end;

procedure TModel.SetEventUrl(const Value: string);
begin
  FEventUrl := Value;
end;

procedure TModel.SetLocalDate(const Value: string);
begin
  FLocalDate := Value;
end;

procedure TModel.SetLocalTime(const Value: string);
begin
  FLocalTime := Value;
end;
{$ENDREGION}
end.
