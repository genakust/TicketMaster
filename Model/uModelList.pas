unit uModelList;

interface

uses
  System.Generics.Collections;

type

  TModelList<T: class> = class(TObjectList<T>)
  private
    FItemsList: TObjectList<T>;
    function GetItemsList: TObjectList<T>;
    procedure SetItemsList(const Value: TObjectList<T>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aItem: T);
    property ItemsList: TObjectList<T> read GetItemsList write SetItemsList;
  end;

implementation

{$REGION '< TModelList >'}

procedure TModelList<T>.Add(aItem: T);
begin
  FItemsList.Add(aItem);
end;

constructor TModelList<T>.Create;
begin
  inherited;

  FItemsList := TObjectList<T>.Create(true);
end;

destructor TModelList<T>.Destroy;
begin
  if Assigned(FItemsList) then
    FItemsList.Clear;
  FItemsList.Free;

  inherited;
end;

function TModelList<T>.GetItemsList: TObjectList<T>;
begin
  Result := FItemsList;
end;

procedure TModelList<T>.SetItemsList(const Value: TObjectList<T>);
begin
  FItemsList := Value;
end;

{$ENDREGION}

end.
