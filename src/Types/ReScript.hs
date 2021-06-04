module Types.ReScript where

import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           Protolude

data Primitive
  = String
  | Char
  | Boolean
  | Integer
  | Float
  | Unit
  deriving (Eq, Show, Ord)

newtype Tuple
  = Tuple [T]
  deriving (Eq, Show, Ord)

data Record = Map Text T
  deriving (Eq, Show, Ord)

data Variant = Variant
  { name     :: Text
  , variants :: Map Text (Maybe [T])
  }
  deriving (Eq, Show, Ord)


data PolyVariant = PolyVariant
  { name     :: Text
  , variants :: Map Text (Maybe [Text])
  }
  deriving (Eq, Show, Ord)

data T
  = Primitive Primitive
  | Tup Tuple
  | Rec Record
  | Var Variant
  | Polyvar PolyVariant
  deriving (Eq, Show, Ord)
