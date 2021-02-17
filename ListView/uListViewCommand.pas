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
    /// <summary> Function to column sort.
    /// </summary>
    procedure ColumnSort(aListView: TListView; aColumn: TListColumn);
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
    /// <summary> Function to column sort.
    /// </summary>
    procedure ColumnSort(aListView: TListView; aColumn: TListColumn);
  end;

implementation

uses
  uListViewSort;

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
      newItem.Caption:= aEventName;
      newItem.SubItems[0] := aEventUrl;
      newItem.SubItems[1] := aLocalTime;
      newItem.SubItems[2] := aLocalDate;
    finally
      aListView.items.EndUpdate;
    end;
  end;
end;

procedure TListViewCommand.ColumnSort(aListView: TListView; aColumn: TListColumn);
var
  colToSort: integer;
begin
  if not Assigned(aListView) then
    Exit;
  // which colum was clicked?
  colToSort := aColumn.index;
  { determine the sort style }
  if (colToSort = 1) or (colToSort = 2) then
    LvSortStyle := cssAlphaNum
  else
    LvSortStyle := cssNumeric;

  { Call the CustomSort method }
  aListView.CustomSort(@CustomSortProc, aColumn.index - 1);

  { Set the sort order for the column }
  LvSortOrder[aColumn.index] := not LvSortOrder[aColumn.index];
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
