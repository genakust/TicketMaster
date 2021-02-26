unit uJsonUtills;

interface

uses
  uModel, uModelList, System.Generics.Collections;

type
  IGetEventListFromJson = interface
    ['{113BF1C8-B2DC-450F-8284-D698C2D348DC}']
    procedure FillEventList(const aJSONContent: string;
      var aEventList: TObjectList<TModel>);
  end;

  TJsonUtills = class(TInterfacedObject, IGetEventListFromJson)
  public
    constructor Create;
    destructor Destroy; override;
    procedure FillEventList(const aJSONContent: string;
      var aEventList: TObjectList<TModel>);
  end;

implementation

uses
  System.JSON;

{ TJsonUtills }

constructor TJsonUtills.Create;
begin
  inherited;

end;

destructor TJsonUtills.Destroy;
begin

  inherited;
end;

procedure TJsonUtills.FillEventList(const aJSONContent: string;
  var aEventList: TObjectList<TModel>);
var
  jsonResponse, embeddedObj, item, datesObj, startArr: TJSONObject;
  events: TJSONArray;
  eventName, eventUrl, localTime, localDate: string;
  i: integer;
begin
  // Start with JSON.
  jsonResponse := TJSONObject.ParseJSONValue(aJSONContent) as TJSONObject;
  try
    if Assigned(jsonResponse) then
    begin
      // JSON Object.
      embeddedObj := (jsonResponse.GetValue('_embedded') as TJSONObject);
      if Assigned(embeddedObj) then
      begin
        // Events list.
        events := (embeddedObj.GetValue('events') as TJSONArray);
        if Assigned(events) then
        begin
          for i := 0 to events.Count - 1 do
          begin
            // One event.
            item := events.Items[i] as TJSONObject;
            if Assigned(item) then
            begin

              // Get properties.
              eventName := (item.GetValue('name') as TJSONString).ToString;
              eventUrl := (item.GetValue('url') as TJSONString).ToString;

              datesObj := (item.GetValue('dates') as TJSONObject);
              if Assigned(datesObj) then
              begin
                startArr := (datesObj.GetValue('start') as TJSONObject);
                if Assigned(startArr) then
                begin
                  localDate :=
                    (startArr.GetValue('localDate') as TJSONString).ToString;
                  localTime :=
                    (startArr.GetValue('localTime') as TJSONString).ToString;
                end;
              end;
              // Add results into event list
              aEventList.Add(TModel.Create(eventName, eventUrl, localTime, localDate));
            end;
          end;
        end
        else
        begin
          // Error case

        end;
      end
      else
      begin
        // Error case

      end;
    end;
  finally
    jsonResponse.DisposeOf;
  end;
end;

end.
