unit uModel;

interface

type
  /// <summary> Represents an Item.
  /// </summary>
  TModel = class
  private
    FEventName, FEventUrl, FLocalTime, FLocalDate: string;
  public
    constructor Create; overload;
    ///<summary>Set variable values
    /// </summary>
    /// <param name="aEventName">Event name.
    /// </param>
    /// <param name="aEventUrl">Url from event.
    /// </param>
    /// <param name="aLocalTime">Start time (lokal).
    /// </param>
    /// <param name="aLocalDate">Start date (lokal).
    /// </param>
    constructor Create(const aEventName, aEventUrl, aLocalTime,
      aLocalDate: string); overload;
    destructor Destroy; override;
    (* Properties *)
    property EventName: string read FEventName write FEventName;
    property EventUrl: string read FEventUrl write FEventUrl;
    property LocalTime: string read FLocalTime write FLocalTime;
    property LocalDate: string read FLocalDate write FLocalDate;
  end;


implementation

uses
  System.SysUtils;

{$REGION '< TModel >'}

constructor TModel.Create(const aEventName, aEventUrl, aLocalTime,
  aLocalDate: string);
begin
  FEventName := aEventName;
  FEventUrl := aEventUrl;
  FLocalTime := aLocalTime;
  FLocalDate := aLocalDate;
end;

constructor TModel.Create;
begin
  inherited;
  FEventName := EmptyStr;
  FEventUrl := EmptyStr;
  FLocalTime := EmptyStr;
  FLocalDate := EmptyStr;
end;

destructor TModel.Destroy;
begin

  inherited;
end;

{$ENDREGION}

end.
