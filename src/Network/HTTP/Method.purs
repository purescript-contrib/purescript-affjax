module Network.HTTP.Method where

import Prelude

data Method
  = DELETE
  | GET
  | HEAD
  | OPTIONS
  | PATCH
  | POST
  | PUT
  | MOVE
  | COPY
  | CustomMethod String

instance eqMethod :: Eq Method where
  eq DELETE  DELETE  = true
  eq GET     GET     = true
  eq HEAD    HEAD    = true
  eq OPTIONS OPTIONS = true
  eq PATCH   PATCH   = true
  eq POST    POST    = true
  eq PUT     PUT     = true
  eq MOVE    MOVE    = true
  eq COPY    COPY     = true
  eq _       _       = false

instance showMethod :: Show Method where
  show DELETE  = "DELETE"
  show GET     = "GET"
  show HEAD    = "HEAD"
  show OPTIONS = "OPTIONS"
  show PATCH   = "PATCH"
  show POST    = "POST"
  show PUT     = "PUT"
  show MOVE    = "MOVE"
  show COPY    = "COPY"
  show (CustomMethod m) = "(CustomMethod " ++ show m ++ ")"

methodToString :: Method -> String
methodToString (CustomMethod m) = m
methodToString other = show other
