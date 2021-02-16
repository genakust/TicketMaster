unit uMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Vcl.Buttons,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls, System.Threading,
  uResourceStrings, uController, Vcl.ComCtrls, System.Actions, Vcl.ActnList;

type
  TForm1 = class(TForm)
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    btnSearch: TSpeedButton;
    panSearch: TPanel;
    edSearchWord: TEdit;
    panActivityPanel: TPanel;
    labMessageText: TLabel;
    ProgressBar: TProgressBar;
    tmrProgress: TTimer;
    ActionList1: TActionList;
    actProgressBarProgress: TAction;
    panDefaults: TPanel;
    labCountry: TLabel;
    cbPlatform: TComboBox;
    Label1: TLabel;
    cbCountry: TComboBox;
    btnSettings: TSpeedButton;
    Panel1: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure tmrProgressTimer(Sender: TObject);
    procedure actProgressBarProgressExecute(Sender: TObject);
  private
    FController: TController;
    FErrorText: string;
    procedure ShowActivityPanel(const MessageText: string);
    procedure HideActivityPanel;
    procedure GetListBySuccess(aJSONContent: string);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  System.JSON;

{$R *.dfm}
{$REGION '< Form Create/Show/Destroy >'}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Is here because DM- module should be created first.
  FController := TController.Create;
end;

{$ENDREGION}
{$REGION '< Get data from request >'}

procedure TForm1.GetListBySuccess(aJSONContent: string);
var
  jsonResponse, embeddedObj, item, datesObj, startArr: TJSONObject;
  events: TJSONArray;
  eventName, eventUrl, localTime, localDate: string;
  i: integer;
begin
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

procedure TForm1.btnSearchClick(Sender: TObject);
var
  newTask: ITask;
begin
  RESTClient.BaseURL := FController.GetJSONRequestForSearch(edSearchWord.Text,
    cbPlatform.Items[cbPlatform.ItemIndex],
    cbCountry.Items[cbCountry.ItemIndex]);

  FErrorText := EmptyStr;
  // Show panel for busy state.
  ShowActivityPanel(rsIsBusy);
  // Try to get a request.
  newTask := TTask.Create(
    procedure
    begin
      RESTRequest.Execute;
      try
        if (RESTResponse.StatusCode = 200) then
        begin
          // Request successful.
          GetListBySuccess(RESTResponse.Content);
        end
        else
        begin
          // Error handling.
          FErrorText := '';
        end;
      except
        on E: Exception do
        begin
          FErrorText := rsError + RESTResponse.ErrorMessage + ' ' + rsError +
            E.Message;
        end;
      end;

      TThread.Synchronize(nil,
        procedure
        begin
          HideActivityPanel;
        end);
    end);
  newTask.Start;

end;
{$ENDREGION}
{$REGION '< Actionslist >'}

procedure TForm1.actProgressBarProgressExecute(Sender: TObject);
const
  kPROGRESS: integer = 10;
begin
  if ProgressBar.Position < ProgressBar.Max then
    ProgressBar.Position := ProgressBar.Position + kPROGRESS
  else
    ProgressBar.Position := ProgressBar.Min;
end;
{$ENDREGION}
{$REGION '< Timer >'}

procedure TForm1.tmrProgressTimer(Sender: TObject);
begin
  // Progress bar
  actProgressBarProgressExecute(nil);
end;
{$ENDREGION}
{$REGION '< Activity panel >'}

procedure TForm1.ShowActivityPanel(const MessageText: string);
begin
  panActivityPanel.Alignment := TAlignment.taCenter;
  labMessageText.Caption := MessageText;
  panActivityPanel.Visible := True;
  // start progress bar
  tmrProgress.Enabled := True;
end;

procedure TForm1.HideActivityPanel;
begin
  panActivityPanel.Visible := False;
  // stop and reset progress bar
  tmrProgress.Enabled := False;
  ProgressBar.Position := ProgressBar.Min;
end;

{$ENDREGION}

initialization

ReportMemoryLeaksOnShutdown := True;

end.
