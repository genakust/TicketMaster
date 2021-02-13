unit uTiketmasterApi;

interface

type
  TTiletmasterApi = class
  private const
    FBaseUrl: string = '';
    function GetBaseURL: string;
  public
    constructor Create;
    destructor Destroy; override;
    property BaseUrl: string read GetBaseURL;
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

function TTiletmasterApi.GetBaseURL: string;
begin
  Result := FBaseUrl;
end;
{$ENDREGION}

end.
