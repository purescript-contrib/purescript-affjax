module Network.HTTP.Method where

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
  (==) DELETE  DELETE  = true
  (==) GET     GET     = true
  (==) HEAD    HEAD    = true
  (==) OPTIONS OPTIONS = true
  (==) PATCH   PATCH   = true
  (==) POST    POST    = true
  (==) PUT     PUT     = true
  (==) MOVE    MOVE    = true
  (==) COPY    COPY     = true
  (==) _       _       = false
  (/=) x y = not (x == y)

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
