module Types.Rust where

import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           Protolude

newtype Identifier = Identifier Text deriving (Eq, Show, Ord)
newtype CIdentifier = CIdentifier Text deriving (Eq, Show, Ord)

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
  deriving (Eq, Show)

data Primitive
  = Bool
  | Char
  | Str
  | String
  | Num Number
  | NumSlice Number
  | JsValue
  | JsValueSlice
  | Unit
  deriving (Eq, Show)

newtype Tuple
  = Tuple [T]
  deriving (Eq, Show)

data Enum = Enum
  { name     :: CIdentifier
  , variants :: Map Identifier (Maybe [T])
  }
  deriving (Eq, Show)

data Struct
  = Struct
    { name   :: CIdentifier
    , fields :: Map Identifier T
    }
  | TupleStruct
    { name :: CIdentifier
    , types :: Tuple
    }
    deriving (Eq, Show)

data T
  = Primitive Primitive
  | Struc Struct
  | Ref T
  | Tup Tuple
  deriving (Eq, Show)
