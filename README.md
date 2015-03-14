# Module Documentation

## Module Network.Affjax

#### `runAffjax`

``` purescript
runAffjax :: forall e a. AffjaxRequest a -> Aff (ajax :: Ajax | e) AjaxResponse
```



## Module Network.Affjax.DSL

#### `AffjaxRequest`

``` purescript
type AffjaxRequest = FreeC AffjaxRequestF
```

A free monad for building AJAX requests

#### `AffjaxRequestF`

``` purescript
data AffjaxRequestF a
  = SetURL String a
  | SetMethod MethodName a
  | AddHeader Header a
  | SetContent (Maybe Content) a
  | SetUsername (Maybe String) a
  | SetPassword (Maybe String) a
```

The request DSL AST.

#### `affjaxRequest`

``` purescript
affjaxRequest :: forall a. AffjaxRequest a -> AjaxRequest
```

Runs the DSL, producing an `AjaxRequest` object.

#### `url`

``` purescript
url :: String -> AffjaxRequest Unit
```

Sets the URL for a request.

#### `method`

``` purescript
method :: Verb -> AffjaxRequest Unit
```

Sets the request method based on an HTTP verb.

#### `method'`

``` purescript
method' :: MethodName -> AffjaxRequest Unit
```

Sets the request method.

#### `header`

``` purescript
header :: HeaderHead -> String -> AffjaxRequest Unit
```

Adds a header to the request using a key and value.

#### `header'`

``` purescript
header' :: Header -> AffjaxRequest Unit
```

Adds a header to the request using a `Header` record.

#### `content`

``` purescript
content :: Content -> AffjaxRequest Unit
```

Sets the content for the request.

#### `content'`

``` purescript
content' :: Maybe Content -> AffjaxRequest Unit
```

Sets the content for the request, with the option of setting it to
`Nothing`.

#### `username`

``` purescript
username :: String -> AffjaxRequest Unit
```

Sets the username for the request.

#### `username'`

``` purescript
username' :: Maybe String -> AffjaxRequest Unit
```

Sets the username for the request, with the option of setting it to
`Nothing`.

#### `password`

``` purescript
password :: String -> AffjaxRequest Unit
```

Sets the password for the request.

#### `password'`

``` purescript
password' :: Maybe String -> AffjaxRequest Unit
```

Sets the password for the request, with the option of setting it to
`Nothing`.


## Module Network.Affjax.Request

#### `Ajax`

``` purescript
data Ajax :: !
```

The event type for AJAX requests.

#### `AjaxRequest`

``` purescript
type AjaxRequest = { password :: Maybe String, username :: Maybe String, content :: Maybe Content, headers :: [Header], method :: MethodName, url :: String }
```

The parameters for an AJAX request.

#### `MethodName`

``` purescript
newtype MethodName
  = MethodName String
```

A HTTP method name: `GET`, `POST`, etc.

#### `Content`

``` purescript
data Content
  = Content String
```

#### `AjaxResponse`

``` purescript
newtype AjaxResponse
```

#### `defaultRequest`

``` purescript
defaultRequest :: AjaxRequest
```

A basic request, `GET /` with no particular headers or credentials.

#### `ajax`

``` purescript
ajax :: forall e. AjaxRequest -> Aff (ajax :: Ajax | e) AjaxResponse
```

Make an AJAX request.



