module Codegen where

import           Protolude
import           Types.ReScript
import qualified Types.ReScript                as ReScript
import           Types.Rust
import qualified Types.Rust                    as Rust

convert :: Rust.T -> ReScript.T
convert (Rust.Primitive p          ) = ReScript.Primitive $ case p of
  Num F32 -> Float
  Num F64 -> Float
  Num _   -> Integer
  Str           -> ReScript.String
  Rust.String   -> ReScript.String
  Rust.Bool     -> ReScript.Boolean
