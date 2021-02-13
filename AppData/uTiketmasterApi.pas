unit uTiketmasterApi;

interface

type
  TTiletmasterApi = class
  private const
    FBaseUrl: string = 'https://app.ticketmaster.com/discovery/v2/events.json?';
    FApiKey: string = 'apikey=';
    FAnd: string = '&';
    function GetBaseURL: string;
    function GetApiKey: string;
    function GetAnd: string;
  public
    constructor Create;
    destructor Destroy; override;
    property BaseUrl: string read GetBaseURL;
    ///<summary>Text for api key. The key is needed
    /// </summary>
    property ApiKey: string read GetApiKey;
    ///<summary>It gets the $ symbol
    /// </summary>
    property AndChar: string read GetAnd;
  end;

implementation

{ TTiletmasterApi }

{$REGION '< Create/Destroy >'}

constructor TTiletmasterApi.Create;
begin
  inherited;

end;

destructor TTiletmasterApi.Destroy;
begin

  inherited;
end;
{$ENDREGION}
{$REGION '< Properties >'}

function TTiletmasterApi.GetAnd: string;
begin
  Result:= FAnd;
end;

function TTiletmasterApi.GetApiKey: string;
begin
  result:= FApiKey;
end;

function TTiletmasterApi.GetBaseURL: string;
begin
  Result := FBaseUrl;
end;
{$ENDREGION}

end.
