unit uListViewCommand;

interface

uses
  Vcl.ComCtrls;

type
  IListViewCommand = interface
    ['{A430B233-1207-416C-85B3-AE2CD44DD60E}']
    /// <summary> Add an item to a given listView.
    /// </summary>
    /// <param name="aEventName">A new event name.
    /// </param>
    /// <param name="aEventUrl">A new event url.
    /// </param>
    /// <param name="aLocalTime">A new local time.
    /// </param>
    /// <param name="aLocalDate">A new local date.
    /// </param>
    /// <param name="aListView">A ListView to add an new item.
    /// </param>
    /// <remarks> If ListView is null it is nothing to happen
    /// </remarks>
    procedure AddItemToList(const aEventName, aEventUrl, aLocalTime,
      aLocalDate: string; aListView: TListView);
    procedure DeleteSelectedItem(aListView: TListView);
  end;

  TListViewCommand = class(TInterfacedObject, IListViewCommand)
  private

  public
    constructor Create;
    destructor Destroy; override;
    /// <summary> Add an item to a given listView.
    /// </summary>
    /// <param name="aEventName">A new event name.
    /// </param>
    /// <param name="aEventUrl">A new event url.
    /// </param>
    /// <param name="aLocalTime">A new local time.
    /// </param>
    /// <param name="aLocalDate">A new local date.
    /// </param>
    /// <param name="aListView">A ListView to add an new item.
    /// </param>
    /// <remarks> If ListView is null it is nothing to happen
    /// </remarks>
    procedure AddItemToList(const aEventName, aEventUrl, aLocalTime,
      aLocalDate: string; aListView: TListView);
    procedure DeleteSelectedItem(aListView: TListView);
  end;

implementation

{ TListViewCommand }

procedure TListViewCommand.AddItemToList(const aEventName, aEventUrl,
  aLocalTime, aLocalDate: string; aListView: TListView);
var
  newItem: TListItem;
  I: integer;
begin
  if (Assigned(aListView)) then
  begin
    aListView.items.BeginUpdate;
    try
      newItem := aListView.items.Add;
      for I := 0 to 2 do
        newItem.SubItems.Add('');
//      newItem.ImageIndex := aImageIndex;
      newItem.SubItems[0] := aEventName;
      newItem.SubItems[1] := aEventUrl;
      newItem.SubItems[2] := aLocalTime;
      newItem.SubItems[2] := aLocalDate;
    finally
      aListView.items.EndUpdate;
    end;
  end;
end;

constructor TListViewCommand.Create;
begin
  inherited;
end;

procedure TListViewCommand.DeleteSelectedItem(aListView: TListView);
begin
  if (Assigned(aListView)) and (aListView.SelCount <> 0) then
    aListView.DeleteSelected;
end;

destructor TListViewCommand.Destroy;
begin

  inherited;
end;

end.
