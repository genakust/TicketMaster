unit uController;

interface

uses
  uTiketmasterApi, uAppData;

type
  TController = class
  private
    FApiStrings: TTiletmasterApi;
    FTokenObj: IAppData;
    FToken: string;
    function GetToken: string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetBaseURL: string;
    property Token : string read GetToken;
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

function TController.GetBaseURL: string;
begin
  Result:= FApiStrings.BaseUrl;
end;

{$REGION '< Properties >'}
function TController.GetToken: string;
begin
  Result:= FToken;
end;
{$ENDREGION}
end.
