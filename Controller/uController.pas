unit uController;

interface

uses
  uTiketmasterApi, uAppData, uJsonUtills, uModel, uModelList, uGK.Logger;

type

  TApiKeyWords = record

  end;

  TController = class
  private
    FApiStrings: TTiletmasterApi;
    FTokenObj: IAppData;
    FToken: string;
    FJsonHandleObj: TJsonUtills;
    FLogger: TLogger;
    procedure CreateFields;
  public
    constructor Create; overload;
    constructor Create(aLogger: TLogger); overload;
    destructor Destroy; override;
    function GetJSONRequestForSearch(const aSearchWord, aPlatform,
      aCountryCode: string): string;
    /// <summary> Fill event list from request.
    /// </summary>
    procedure FillEventListBySuccess(const aJSONContent: string;
      var aEventList: TModelList<TModel>);
  end;

implementation

uses
  System.SysUtils;

{ TController }
{$REGION '< Create/Show/Destroy >'}

constructor TController.Create(aLogger: TLogger);
begin
  CreateFields;
  // Logger object to log errors
  FLogger := aLogger;
end;

constructor TController.Create;
begin
  inherited;
  CreateFields;
  // Logger object to log errors
  FLogger := nil;;
end;

destructor TController.Destroy;
begin
  FApiStrings.Free;
  FJsonHandleObj.Free;

  inherited;
end;

procedure TController.FillEventListBySuccess(const aJSONContent: string;
  var aEventList: TModelList<TModel>);
begin
  try
    // Fill the list.
    FJsonHandleObj.FillEventList(aJSONContent, aEventList);
  except
    on  E: Exception do
    begin
    // Handle exception.
      if Assigned(FLogger) then
        FLogger.Log(E.Message);
    end
  end;
end;

procedure TController.CreateFields;
begin
  FApiStrings := TTiletmasterApi.Create;
  FTokenObj := TAppData.GetInstance;
  FToken := FTokenObj.Token;
  FJsonHandleObj := TJsonUtills.Create;
end;

function TController.GetJSONRequestForSearch(const aSearchWord, aPlatform,
  aCountryCode: string): string;
begin
  result := FApiStrings.GetJSONRequestForSearch(aSearchWord, aPlatform,
    aCountryCode, FToken);
end;

{$ENDREGION}

end.
