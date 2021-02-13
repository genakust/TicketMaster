unit uController;

interface

uses
  uTiketmasterApi;

type
  TController = class
  private
    FApiStrings: TTiletmasterApi;
  public
    constructor Create;
    destructor Destroy; override;
    function GetBaseURL: string;
  end;

implementation

{ TController }
{$REGION '< Create/Show/Destroy >'}

constructor TController.Create;
begin
  inherited;

  FApiStrings:= TTiletmasterApi.Create;
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

end.
