unit uMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Vcl.Buttons, uAppData,
  uController, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    SpeedButton1: TSpeedButton;
    Panel1: TPanel;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FController: TController;
    FTokenObj: TAppData;
    FToken: string;
    FErrorText: string;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  FController := TController.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTokenObj.Free;
  FController.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  RESTClient.BaseURL := FController.GetBaseURL +
    'classificationName=music&dmaId=324&apikey={apikey}';
  FTokenObj := TAppData.GetInstance;
  FToken := FTokenObj.Token;
  RESTClient.BaseURL := FController.GetBaseURL +
    'classificationName=music&dmaId=324&apikey=' + FToken;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  jValue: TJSONValue;
begin
  FErrorText:= EmptyStr;
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
      FErrorText:= '';
    end;
  except
    on E: Exception do
    begin
      FErrorText:= 'Error: ' + RESTResponse.ErrorMessage + ' Error: ' + E.Message;
    end;

  end;
end;

{$ENDREGION}

initialization

ReportMemoryLeaksOnShutdown := true;

end.
