object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 150
  Width = 215
  object FDConnection: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\genak\OneDrive\Dokumente\Embarcadero\Studio\Pr' +
        'ojekte\TicketMaster\DM\Database.sqlite'
      'DriverID=SQLite')
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
