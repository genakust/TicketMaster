object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 150
  Width = 215
  object FDConnection: TFDConnection
    Params.Strings = (
      
        'Database=D:\Dokumente\Embarcadero\Studio\Projekte\TicketMaster\D' +
        'M\Database.sqlite'
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    Left = 24
    Top = 72
  end
  object FDQGetToken: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'select * from Appdata')
    Left = 96
    Top = 72
  end
end
