unit uTiketmasterApi;

interface
(*
  > Documentation
  https://developer.ticketmaster.com/products-and-docs/apis/discovery-api/v2/

  > Get a list of all events in the United States:
  https://app.ticketmaster.com/discovery/v2/events.json?countryCode=US&apikey={apikey}

  > Search for events sourced by Universe in the United States with keyword “devjam”:
  https://app.ticketmaster.com/discovery/v2/events.json?keyword=devjam&source=universe&
    countryCode=US&apikey={apikey}
*)
type
  ///<summary>Events are available from the following countries:
  /// </summary>
  TSupportedCountries  = (US, CA, IE, GB, AU, NZ, MX, AT, BE, DE, DK, ES, FI, NL, NO, PL, SE, CH, CZ, IT, FR);

  TTiletmasterApi = class
  private const
    FBaseUrl: string = 'https://app.ticketmaster.com/discovery/v2/events.json?';
    FApiKey: string = 'apikey=';
    FAnd: string = '&';
    FCountryCode: string = 'countryCode=';
    FKeyword:string = 'keyword=';
  private
    function GetBaseURL: string;
    function GetApiKey: string;
    function GetAnd: string;
    function GetCountryCode: string;
    function GetKeyword: string;
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
    property CountryCode : string read GetCountryCode;
    property Keyword : string read GetKeyword;
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
function TTiletmasterApi.GetCountryCode: string;
begin
  Result:= FCountryCode;
end;

function TTiletmasterApi.GetKeyword: string;
begin
  Result:= FKeyword;
end;

{$ENDREGION}

end.
