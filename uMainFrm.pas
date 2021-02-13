unit uMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, Vcl.Buttons, uAppData;

type
  TForm1 = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    SpeedButton1: TSpeedButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FTokenObj: TAppData;
    FToken: string;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$REGION '< Form Create/Show/Destroy >'}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTokenObj.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FTokenObj := TAppData.GetInstance;
  FToken := FTokenObj.Token;
end;
{$ENDREGION}

initialization

ReportMemoryLeaksOnShutdown := true;

end.
