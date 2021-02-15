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
    function GetToken: string;
    function GetBaseURL: string;
    function GetKeyWord: string;
    function GetAnd: string;
    function GetSource: string;
    function GetCountryCode: string;
    function GetApiKey: string;
  public
    constructor Create;
    destructor Destroy; override;
    (*Properties*)
    property KeyWord: string read GetKeyWord;
    property BaseURL: string read GetBaseURL;
    property Token : string read GetToken;
    property AndChar: string read GetAnd ;
    property Source: string read GetSource ;
    property CountryCode: string read GetCountryCode ;
    property ApiKey: string read GetApiKey;
  end;

implementation

{ TController }
{$REGION '< Create/Show/Destroy >'}

constructor TController.Create;
begin
  inherited;

  FApiStrings:= TTiletmasterApi.Create;
  FTokenObj := TAppData.GetInstance;
  FToken := FTokenObj.Token;
end;

destructor TController.Destroy;
begin
  FApiStrings.Free;

  inherited;
end;

{$ENDREGION}

{$REGION '< Properties >'}
function TController.GetAnd: string;
begin
  Result:= FApiStrings.AndChar;
end;

function TController.GetApiKey: string;
begin
  Result:= FApiStrings.ApiKey;
end;

function TController.GetBaseURL: string;
begin
  Result:= FApiStrings.BaseUrl;
end;

function TController.GetCountryCode: string;
begin
  Result:= FApiStrings.CountryCode;
end;

function TController.GetKeyWord: string;
begin
  Result:= FApiStrings.Keyword;
end;

function TController.GetSource: string;
begin
  Result:= FApiStrings.Source;
end;

function TController.GetToken: string;
begin
  Result:= FToken;
end;
{$ENDREGION}
end.
