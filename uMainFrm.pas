unit uMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Vcl.Buttons,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls, System.Threading,
  uResourceStrings, uController;

type
  TForm1 = class(TForm)
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    SpeedButton1: TSpeedButton;
    Panel1: TPanel;
    Edit1: TEdit;
    Memo1: TMemo;
    panActivityPanel: TPanel;
    labMessageText: TLabel;
    ActivityIndicator: TActivityIndicator;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FController: TController;
    FErrorText: string;
    procedure ShowActivityPanel(const MessageText: string);
    procedure HideActivityPanel;
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
  RESTClient.BaseURL := FController.GetBaseURL +
    'classificationName=music&dmaId=324&apikey=' + FController.Token;
end;

{$ENDREGION}

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  jValue: TJSONValue;
  newTask: ITask;
begin
  FErrorText := EmptyStr;
  ShowActivityPanel(rsIsBusy);
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

{$REGION '< Activity panel >'}

procedure TForm1.ShowActivityPanel(const MessageText: string);
begin
  panActivityPanel.Alignment := TAlignment.taCenter;
  labMessageText.Caption := MessageText;
  ActivityIndicator.Enabled := True;
  panActivityPanel.Visible := True;
end;

procedure TForm1.HideActivityPanel;
begin
  ActivityIndicator.Enabled := False;
  panActivityPanel.Visible := False;
end;

{$ENDREGION}

initialization

ReportMemoryLeaksOnShutdown := True;

end.
