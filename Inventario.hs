import qualified Data.Map as Map
import Data.Time (UTCTime)

data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read)
  
type Inventario = Data.Map.Map String Item

data AcaoLog
    = Add
    | Remove
    | Update
    | QueryFail
    deriving (Show, Read)

data StatusLog
    = Sucesso
    | Falha String
    deriving (Show, Read)

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao :: AcaoLog
    , detalhes :: String
    , status :: StatusLog
    } deriving (Show, Read)

