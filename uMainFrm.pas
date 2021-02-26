unit uMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Vcl.Buttons,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls, System.Threading,
  uResourceStrings, uController, Vcl.ComCtrls, System.Actions, Vcl.ActnList,
  uGK.Logger, System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors,
  Data.Bind.EngExt, Vcl.Bind.DBEngExt, uModel, uModelList, uListViewCommand,
  uListViewSort, Vcl.Bind.GenData, Data.Bind.GenData;

type
  TfrmTicketmaster = class(TForm)
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    btnSearch: TSpeedButton;
    panSearch: TPanel;
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
    cbSearchWord: TComboBox;
    actAddSearchWordsToList: TAction;
    StatusBar1: TStatusBar;
    actStartRestRequest: TAction;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure tmrProgressTimer(Sender: TObject);
    procedure actProgressBarProgressExecute(Sender: TObject);
    procedure lvEventsListColumnClick(Sender: TObject; Column: TListColumn);
    procedure actAddSearchWordsToListExecute(Sender: TObject);
    procedure cbSearchWordKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actStartRestRequestExecute(Sender: TObject);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
  private
    FController: TController;
    FLogger: TLogger;
    FErrorText: string;
    FEventList: TObjectList<TModel>;
    procedure ShowActivityPanel(const MessageText: string);
    procedure HideActivityPanel;
    procedure GetListBySuccess(aJSONContent: string);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmTicketmaster: TfrmTicketmaster;

implementation

{$R *.dfm}
{$REGION '< Form Create/Show/Destroy >'}

procedure TfrmTicketmaster.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TfrmTicketmaster.FormShow(Sender: TObject);
begin
  // Create a logger.
  FLogger := TLogger.GetInstance;
  // Is here because DM- module should be created first.
  FController := TController.Create(FLogger);
end;

constructor TfrmTicketmaster.Create(AOwner: TComponent);
begin
  FEventList := TObjectList<TModel>.Create(true);
  FEventList.OwnsObjects := true;
  inherited;
end;

{$ENDREGION}
{$REGION '< Get data from request >'}

procedure TfrmTicketmaster.GetListBySuccess(aJSONContent: string);
begin
  try
    if FEventList.Count > 0 then
      FEventList.Clear;
    FController.FillEventListBySuccess(RESTResponse.Content, FEventList);
    BindingsList1.Notify(FEventList, '');
  finally

  end;
end;

procedure TfrmTicketmaster.btnSearchClick(Sender: TObject);
begin
  actStartRestRequestExecute(nil);
end;

{$ENDREGION}
{$REGION '< Actionslist >'}

procedure TfrmTicketmaster.actAddSearchWordsToListExecute(Sender: TObject);
var
  text: string;
  ind: integer;
begin
  cbSearchWord.Items.BeginUpdate;
  try
    text := cbSearchWord.text;
    ind := cbSearchWord.Items.Add(text);
    cbSearchWord.ItemIndex := ind;
  finally
    cbSearchWord.Items.EndUpdate;
  end;
end;

procedure TfrmTicketmaster.actProgressBarProgressExecute(Sender: TObject);
const
  kPROGRESS: integer = 10;
begin
  if ProgressBar.Position < ProgressBar.Max then
    ProgressBar.Position := ProgressBar.Position + kPROGRESS
  else
    ProgressBar.Position := ProgressBar.Min;
end;

procedure TfrmTicketmaster.actStartRestRequestExecute(Sender: TObject);
var
  newTask: ITask;
begin
  RESTClient.BaseURL := FController.GetJSONRequestForSearch
    (cbSearchWord.Items[cbSearchWord.ItemIndex],
    cbPlatform.Items[cbPlatform.ItemIndex],
    cbCountry.Items[cbCountry.ItemIndex]);

  FErrorText := EmptyStr;
  lvEventsList.Clear;

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
          FErrorText := RESTResponse.StatusCode.ToString;
          // Log error message into debug window.
          FLogger.Log(FErrorText);
        end;
      except
        on E: Exception do
        begin
          FErrorText := rsError + ' ' + RESTResponse.ErrorMessage + ' ' +
            rsError + ' ' + E.Message;
          // Log error message into debug window.
          FLogger.Log(FErrorText);
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
  panActivityPanel.Visible := true;
  // start progress bar
  tmrProgress.Enabled := true;
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

procedure TfrmTicketmaster.lvEventsListColumnClick(Sender: TObject;
Column: TListColumn);
var
  lvCmd: IListViewCommand;
begin
  lvCmd := TListViewCommand.Create;
  lvCmd.ColumnSort(lvEventsList, Column);
end;

procedure TfrmTicketmaster.PrototypeBindSource1CreateAdapter(Sender: TObject;
var ABindSourceAdapter: TBindSourceAdapter);
var
  model: TModel;
begin
  model := TModel.Create('name1', 'url1', 'date1', 'time1');
  FEventList.Add(model);
  model := TModel.Create('name2', 'url2', 'date2', 'time2');
  FEventList.Add(model);
  ABindSourceAdapter := TListBindSourceAdapter<TModel>.Create(self,
    FEventList, true);
end;

{$ENDREGION}
{$REGION '< ComboBox SearchWord >'}

procedure TfrmTicketmaster.cbSearchWordKeyUp(Sender: TObject; var Key: Word;
Shift: TShiftState);
begin
  if ord(Key) = VK_RETURN then
  begin
    actAddSearchWordsToListExecute(nil);
  end;
end;

{$ENDREGION}

initialization

ReportMemoryLeaksOnShutdown := true;

end.
