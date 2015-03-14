# Module Documentation

## Module Network.Affjax

#### `runAffjax`

``` purescript
runAffjax :: forall e c a. AffjaxRequest c a -> Aff (ajax :: Ajax | e) AjaxResponse
```



## Module Network.Affjax.DSL

#### `AffjaxRequest`

``` purescript
type AffjaxRequest c = FreeC (AffjaxRequestF c)
```

A free monad for building AJAX requests

#### `AffjaxRequestF`

``` purescript
data AffjaxRequestF c a
  = SetURL String a
  | SetMethod MethodName a
  | AddHeader Header a
  | SetContent (Maybe (Content c)) a
  | SetUsername (Maybe String) a
  | SetPassword (Maybe String) a
```

The request DSL AST.

#### `affjaxRequest`

``` purescript
affjaxRequest :: forall c a. AffjaxRequest c a -> AjaxRequest c
```

Runs the DSL, producing an `AjaxRequest` object.

#### `url`

``` purescript
url :: forall c. String -> AffjaxRequest c Unit
```

Sets the URL for a request.

#### `method`

``` purescript
method :: forall c. Verb -> AffjaxRequest c Unit
```

Sets the request method based on an HTTP verb.

#### `method'`

``` purescript
method' :: forall c. MethodName -> AffjaxRequest c Unit
```

Sets the request method.

#### `header`

``` purescript
header :: forall c. HeaderHead -> String -> AffjaxRequest c Unit
```

Adds a header to the request using a key and value.

#### `header'`

``` purescript
header' :: forall c. Header -> AffjaxRequest c Unit
```

Adds a header to the request using a `Header` record.

#### `content`

``` purescript
content :: forall c. Content c -> AffjaxRequest c Unit
```

Sets the content for the request.

#### `content'`

``` purescript
content' :: forall c. Maybe (Content c) -> AffjaxRequest c Unit
```

Sets the content for the request, with the option of setting it to
`Nothing`.

#### `username`

``` purescript
username :: forall c. String -> AffjaxRequest c Unit
```

Sets the username for the request.

#### `username'`

``` purescript
username' :: forall c. Maybe String -> AffjaxRequest c Unit
```

Sets the username for the request, with the option of setting it to
`Nothing`.

#### `password`

``` purescript
password :: forall c. String -> AffjaxRequest c Unit
```

Sets the password for the request.

#### `password'`

``` purescript
password' :: forall c. Maybe String -> AffjaxRequest c Unit
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
type AjaxRequest a = { password :: Maybe String, username :: Maybe String, content :: Maybe (Content a), headers :: [Header], method :: MethodName, url :: String }
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
data Content a
  = ArrayViewContent (ArrayView a)
  | BlobContent Blob
  | DocumentContent Document
  | TextContent String
  | FormDataContent FormData
```

#### `AjaxResponse`

``` purescript
newtype AjaxResponse
```

#### `defaultRequest`

``` purescript
defaultRequest :: forall c. AjaxRequest c
```

A basic request, `GET /` with no particular headers or credentials.

#### `ajax`

``` purescript
ajax :: forall e a. AjaxRequest a -> Aff (ajax :: Ajax | e) AjaxResponse
```

Make an AJAX request.



