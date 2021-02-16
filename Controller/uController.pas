unit uController;

interface

uses
  uTiketmasterApi, uAppData;

type

  TApiKeyWords = record

  end;

  TController = class
  private
    FApiStrings: TTiletmasterApi;
    FTokenObj: IAppData;
    FToken: string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetJSONRequestForSearch(const aSearchWord, aPlatform,
      aCountryCode: string): string;
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
end;

destructor TController.Destroy;
begin
  FApiStrings.Free;

  inherited;
end;

function TController.GetJSONRequestForSearch(const aSearchWord, aPlatform,
  aCountryCode: string): string;
begin
  Result := FApiStrings.GetJSONRequestForSearch(aSearchWord, aPlatform,
    aCountryCode, FToken);
end;

{$ENDREGION}

end.
