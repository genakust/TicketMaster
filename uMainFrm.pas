unit uMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
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
    Memo1: TMemo;
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
    procedure GetListBySuccess;
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
procedure TForm1.GetListBySuccess;
var
  Value: TJSONValue;
begin
  Value := TJSONObject.ParseJSONValue( RESTResponse.Content );
  try

  finally

  end;
end;

procedure TForm1.btnSearchClick(Sender: TObject);
var
  jValue: TJSONValue;
  newTask: ITask;
begin
  // URL.
  (* https://app.ticketmaster.com/discovery/v2/events.json?
    keyword=disco&
    source=Ticketmaster&
    countryCode=US&
    apikey={} *)

  RESTClient.BaseURL :=
    FController.BaseURL +
    FController.KeyWord + edSearchWord.Text + FController.AndChar +
    FController.Source + cbPlatform.Items[cbPlatform.ItemIndex] + FController.AndChar +
    FController.CountryCode + cbCountry.Items[cbCountry.ItemIndex] + FController.AndChar +
    FController.ApiKey + FController.Token;

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
          jValue := RESTResponse.JSONValue;
          Memo1.Text := jValue.ToString;
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

      TThread.Synchronize( nil,
          procedure
          begin
            HideActivityPanel;
          end );
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
    ProgressBar.Position:= ProgressBar.Position + kPROGRESS
  else
    ProgressBar.Position:= ProgressBar.Min;
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
  tmrProgress.Enabled:= true;
end;

procedure TForm1.HideActivityPanel;
begin
  panActivityPanel.Visible := False;
  // stop and reset progress bar
  tmrProgress.Enabled:= false;
  ProgressBar.Position:= ProgressBar.Min;
end;

{$ENDREGION}

initialization

ReportMemoryLeaksOnShutdown := True;

end.
