module Types.ReScript where

import Data.Map (Map)
import Data.Text (Text)
import Protolude (Maybe)

data Primitive
  = String
  | Char
  | Boolean
  | Integer
  | Float
  | Unit

newtype Tuple
  = Tuple [T]

data Record
  = Map Text T

data Variant
  = Variant { name :: Text
            , variants :: Map Text (Maybe [T])
            }

data PolyVariant
  = PolyVariant { name :: Text
                , variants :: Map Text (Maybe [Text])
                }

data T
  = Primitive Primitive
  | Tup Tuple
  | Rec Record
  | Var Variant
  | Polyvar PolyVariant
