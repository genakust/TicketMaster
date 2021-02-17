unit uController;

interface

uses
  uTiketmasterApi, uAppData, uJsonUtills, uModel, uModelList;

type

  TApiKeyWords = record

  end;

  TController = class
  private
    FApiStrings: TTiletmasterApi;
    FTokenObj: IAppData;
    FToken: string;
    FJsonHandleObj: TJsonUtills;
  public
    constructor Create;
    destructor Destroy; override;
    function GetJSONRequestForSearch(const aSearchWord, aPlatform,
      aCountryCode: string): string;
    /// <summary> Fill event list from request.
    /// </summary>
    procedure FillEventListBySuccess(const aJSONContent: string;
      var aEventList: TModelList<TModel>);
  end;

implementation

{ TController }
{$REGION '< Create/Show/Destroy >'}

constructor TController.Create;
begin
  inherited;

  FApiStrings := TTiletmasterApi.Create;
  FTokenObj := TAppData.GetInstance;
  FToken := FTokenObj.Token;
  FJsonHandleObj := TJsonUtills.Create;
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
    // Handle exception.

  end;
end;

function TController.GetJSONRequestForSearch(const aSearchWord, aPlatform,
  aCountryCode: string): string;
begin
  result := FApiStrings.GetJSONRequestForSearch(aSearchWord, aPlatform,
    aCountryCode, FToken);
end;

{$ENDREGION}

end.
