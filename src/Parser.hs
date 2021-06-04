module Parser where

import           Control.Monad.Fail             ( fail )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void
import           Protolude               hiding ( (<|>)
                                                , many
                                                , try
                                                )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lexer
import           Types.Rust                     ( CIdentifier(..)
                                                , Identifier(..)
                                                , Number(..)
                                                , Primitive(..)
                                                , Struct(..)
                                                , T(..)
                                                , Tuple(..)
                                                )
import qualified Types.Rust                    as Rust

type Parser = Parsec Void Text

surroundedBy p = between p p

parseTable :: [(a, Text)] -> Parser a
parseTable = choice . fmap (try . uncurry (<$) . fmap string)

betweenBraces :: Parser a -> Parser a
betweenBraces =
  between (surroundedBy space (char '{')) (surroundedBy space (char '}'))

betweenParens :: Parser a -> Parser a
betweenParens =
  between (surroundedBy space (char '(')) (surroundedBy space (char ')'))

number :: Parser Rust.Number
number = parseTable
  [ (U8   , "u8")
  , (U16  , "u16")
  , (U32  , "u32")
  , (U64  , "u64")
  , (USIZE, "usize")
  , (I8   , "i8")
  , (I16  , "i16")
  , (I32  , "i32")
  , (I64  , "i64")
  , (ISIZE, "isize")
  , (F32  , "f32")
  , (F64  , "f64")
  ]

primitive :: Parser Rust.Primitive
primitive = parseTable
  [ (Bool   , "bool")
  , (Char   , "char")
  , (Str    , "str")
  , (String , "String")
  , (JsValue, "JsValue")
  ]

identifier :: Parser Rust.Identifier
identifier = do
  start     <- letterChar
  remaining <- many (alphaNumChar <|> char '_')
  return . Identifier . Text.pack $ (start : remaining)

cIdentifier :: Parser Rust.CIdentifier
cIdentifier = do
  start     <- upperChar
  remaining <- many (alphaNumChar <|> char '_')
  return . CIdentifier . Text.pack $ (start : remaining)

pTuple :: Parser Tuple
pTuple =
  Tuple <$> betweenParens (rustType `sepBy` surroundedBy space (char ','))

struct :: Parser Rust.Struct
struct = do
  _      <- string "struct"
  _      <- space1
  name   <- cIdentifier
  _      <- space
  fields <- betweenBraces (field `sepBy` surroundedBy space (char ','))
  return (Struct name (Map.fromList fields))
 where
  field = do
    id <- identifier
    _  <- surroundedBy space (char ':')
    t  <- rustType
    return (id, t)

tupleStruct :: Parser Rust.Struct
tupleStruct = do
  _ <- string "struct"
  _ <- space1
  TupleStruct <$> cIdentifier <*> pTuple

rustType :: Parser Rust.T
rustType =
  choice
    $   try
    <$> [ Primitive . Num <$> number
        , Struc <$> struct
        , Struc <$> tupleStruct
        , Primitive <$> primitive
        , Tup <$> pTuple
        ]
