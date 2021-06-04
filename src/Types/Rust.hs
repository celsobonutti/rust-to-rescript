module Rust where

import Data.Map (Map)
import Data.Text (Text)
import Protolude (Maybe)

data Number
  = U8
  | I8
  | U16
  | I16
  | U32
  | I32
  | U64
  | I64
  | ISIZE
  | USIZE
  | F32
  | F64

newtype Tuple
  = Tuple [T]

data Enum
  = Enum { name :: Text
         , variants :: Map Text (Maybe [T])
         }

data Struct
  = Map Text T

data Primitive
  = Bool
  | Char
  | Str
  | String
  | Num Number
  | NumSlice Number
  | JsValue
  | JsValueSlice

data T
  = Primitive Primitive
  | Struc Struct
