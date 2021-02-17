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
  TfrmTicketmaster = class(TForm)
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
    panListView: TPanel;
    lvEventsList: TListView;
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
    procedure ListViewCreateColumn;
  public
    { Public-Deklarationen }
  end;

var
  frmTicketmaster: TfrmTicketmaster;

implementation

uses
  uModel, uModelList, uListViewCommand;

{$R *.dfm}
{$REGION '< Form Create/Show/Destroy >'}

procedure TfrmTicketmaster.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TfrmTicketmaster.FormShow(Sender: TObject);
begin
  // Is here because DM- module should be created first.
  FController := TController.Create;

  ListViewCreateColumn;
end;

{$ENDREGION}
{$REGION '< Get data from request >'}

procedure TfrmTicketmaster.GetListBySuccess(aJSONContent: string);
var
  eventList: TModelList<TModel>;
  listViewCmd: IListViewCommand;
  item: TModel;
begin
  eventList := TModelList<TModel>.Create;
  try
    listViewCmd:= TListViewCommand.Create;
    FController.FillEventListBySuccess(RESTResponse.Content, eventList);
    for item in eventList.ItemsList do
    begin
      listViewCmd.AddItemToList(item.EventName, item.EventUrl,
        item.LocalTime, item.LocalDate, lvEventsList);
    end;
  finally
    eventList.Free;
  end;
end;

procedure TfrmTicketmaster.btnSearchClick(Sender: TObject);
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

procedure TfrmTicketmaster.actProgressBarProgressExecute(Sender: TObject);
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

procedure TfrmTicketmaster.tmrProgressTimer(Sender: TObject);
begin
  // Progress bar
  actProgressBarProgressExecute(nil);
end;
{$ENDREGION}
{$REGION '< Activity panel >'}

procedure TfrmTicketmaster.ShowActivityPanel(const MessageText: string);
begin
  panActivityPanel.Alignment := TAlignment.taCenter;
  labMessageText.Caption := MessageText;
  panActivityPanel.Visible := True;
  // start progress bar
  tmrProgress.Enabled := True;
end;

procedure TfrmTicketmaster.HideActivityPanel;
begin
  panActivityPanel.Visible := False;
  // stop and reset progress bar
  tmrProgress.Enabled := False;
  ProgressBar.Position := ProgressBar.Min;
end;

{$ENDREGION}
{$REGION '< ListView >'}

procedure TfrmTicketmaster.ListViewCreateColumn;
var
  newCol: TListColumn;
begin
  newCol := lvEventsList.Columns.Add;
  newCol.Caption := 'Event Name';
  newCol.Alignment := taLeftJustify;
  newCol.Width := 100;

  newCol := lvEventsList.Columns.Add;
  newCol.Caption := 'Event Url';
  newCol.Alignment := taLeftJustify;
  newCol.Width := 140;

  newCol := lvEventsList.Columns.Add;
  newCol.Caption := 'Local Time';
  newCol.Alignment := taLeftJustify;
  newCol.Width := 140;

  newCol := lvEventsList.Columns.Add;
  newCol.Caption := 'Local Date';
  newCol.Alignment := taLeftJustify;
  newCol.Width := 140;
end;
{$ENDREGION}

initialization

ReportMemoryLeaksOnShutdown := True;

end.
