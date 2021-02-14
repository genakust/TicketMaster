object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 486
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 48
    Top = 240
    Width = 457
    Height = 41
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 368
      Top = 1
      Width = 88
      Height = 39
      Align = alRight
      OnClick = SpeedButton1Click
    end
    object Edit1: TEdit
      Left = 1
      Top = 1
      Width = 367
      Height = 39
      Align = alClient
      TabOrder = 0
      Text = 'Edit1'
      ExplicitHeight = 21
    end
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 516
    Height = 226
    TabOrder = 1
  end
  object panActivityPanel: TPanel
    Left = 135
    Top = 296
    Width = 217
    Height = 97
    TabOrder = 2
    Visible = False
    object labMessageText: TLabel
      Left = 1
      Top = 1
      Width = 215
      Height = 23
      Align = alTop
      Alignment = taCenter
      Caption = 'Loading'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitWidth = 66
    end
    object ActivityIndicator: TActivityIndicator
      Left = 96
      Top = 48
    end
  end
  object RESTClient: TRESTClient
    Accept = 'application/json'
    Params = <>
    Left = 48
    Top = 408
  end
  object RESTRequest: TRESTRequest
    Client = RESTClient
    Params = <>
    Response = RESTResponse
    SynchronizedEvents = False
    Left = 120
    Top = 408
  end
  object RESTResponse: TRESTResponse
    Left = 184
    Top = 416
  end
end
