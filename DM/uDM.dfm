object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 150
  Width = 215
  object FDConnection: TFDConnection
    Params.Strings = (
      
        'Database=D:\Dokumente\Embarcadero\Studio\Projekte\TicketMaster\D' +
        'M\AppData'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 24
    Top = 72
  end
  object FDQGetToken: TFDQuery
    Connection = FDConnection
    Left = 96
    Top = 72
  end
end
