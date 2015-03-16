module Network.Affjax.HTTP where

data Method
  = DELETE
  | GET
  | HEAD
  | OPTIONS
  | PATCH
  | POST
  | PUT
  | CustomMethod String

instance eqMethod :: Eq Method where
  (==) DELETE  DELETE  = true
  (==) GET     GET     = true
  (==) HEAD    HEAD    = true
  (==) OPTIONS OPTIONS = true
  (==) PATCH   PATCH   = true
  (==) POST    POST    = true
  (==) PUT     PUT     = true
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
  show (CustomMethod m) = "(CustomMethod " ++ show m ++ ")"

newtype HeaderHead = HeaderHead String

instance eqHeaderHead :: Eq HeaderHead where
  (==) (HeaderHead x) (HeaderHead y) = x == y
  (/=) (HeaderHead x) (HeaderHead y) = x /= y

instance showHeaderHead :: Show HeaderHead where
  show (HeaderHead h) = "(HeaderHead " ++ show h ++ ")"

data Header = Header HeaderHead String

instance eqHeader :: Eq Header where
  (==) (Header hx x) (Header hy y) = hx == hy && x == y
  (/=) (Header hx x) (Header hy y) = hx /= hy || x /= y

instance showHeader :: Show Header where
  show (Header hh h) = "(Header " ++ show hh ++ " " ++ show h ++ ")"
