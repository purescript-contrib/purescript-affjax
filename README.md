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
  | SetMethod Method a
  | AddHeader RequestHeader a
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
method :: Method -> AffjaxRequest Unit
```

Sets the request method based on an HTTP verb.

#### `header`

``` purescript
header :: RequestHeader -> AffjaxRequest Unit
```

Adds a header to the request.

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
type AjaxRequest = { password :: Maybe String, username :: Maybe String, content :: Maybe Content, headers :: [RequestHeader], method :: Method, url :: String }
```

The parameters for an AJAX request.

#### `Content`

``` purescript
data Content
  = ArrayViewContent (Exists ArrayView)
  | BlobContent Blob
  | DocumentContent Document
  | TextContent String
  | FormDataContent FormData
```

The types of data that can be set in an AJAX request.

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


## Module Network.HTTP.Method

#### `Method`

``` purescript
data Method
  = DELETE 
  | GET 
  | HEAD 
  | OPTIONS 
  | PATCH 
  | POST 
  | PUT 
  | CustomMethod String
```


#### `eqMethod`

``` purescript
instance eqMethod :: Eq Method
```


#### `showMethod`

``` purescript
instance showMethod :: Show Method
```


#### `methodToString`

``` purescript
methodToString :: Method -> String
```



## Module Network.HTTP.MimeType

#### `MimeType`

``` purescript
newtype MimeType
  = MimeType String
```


#### `eqMimeType`

``` purescript
instance eqMimeType :: Eq MimeType
```


#### `showMimeType`

``` purescript
instance showMimeType :: Show MimeType
```


#### `mimeTypeToString`

``` purescript
mimeTypeToString :: MimeType -> String
```



## Module Network.HTTP.RequestHeader

#### `RequestHeader`

``` purescript
data RequestHeader
  = Accept MimeType
  | ContentType MimeType
  | RequestHeader String String
```


#### `eqRequestHeader`

``` purescript
instance eqRequestHeader :: Eq RequestHeader
```


#### `showRequestHeader`

``` purescript
instance showRequestHeader :: Show RequestHeader
```


#### `requestHeaderName`

``` purescript
requestHeaderName :: RequestHeader -> String
```


#### `requestHeaderValue`

``` purescript
requestHeaderValue :: RequestHeader -> String
```



## Module Network.HTTP.ResponseHeader

#### `ResponseHeader`

``` purescript
data ResponseHeader
  = ResponseHeader String String
```


#### `eqResponseHeader`

``` purescript
instance eqResponseHeader :: Eq ResponseHeader
```


#### `showResponseHeader`

``` purescript
instance showResponseHeader :: Show ResponseHeader
```


#### `responseHeaderName`

``` purescript
responseHeaderName :: ResponseHeader -> String
```


#### `responseHeaderValue`

``` purescript
responseHeaderValue :: ResponseHeader -> String
```



## Module Network.HTTP.StatusCode

#### `StatusCode`

``` purescript
newtype StatusCode
  = StatusCode Int
```


#### `eqStatusCode`

``` purescript
instance eqStatusCode :: Eq StatusCode
```


#### `showStatusCode`

``` purescript
instance showStatusCode :: Show StatusCode
```



## Module Network.HTTP.MimeType.Common

#### `applicationFormURLEncoded`

``` purescript
applicationFormURLEncoded :: MimeType
```


#### `applicationJSON`

``` purescript
applicationJSON :: MimeType
```


#### `applicationJavascript`

``` purescript
applicationJavascript :: MimeType
```


#### `applicationOctetStream`

``` purescript
applicationOctetStream :: MimeType
```


#### `applicationXML`

``` purescript
applicationXML :: MimeType
```


#### `imageGIF`

``` purescript
imageGIF :: MimeType
```


#### `imageJPEG`

``` purescript
imageJPEG :: MimeType
```


#### `imagePNG`

``` purescript
imagePNG :: MimeType
```


#### `multipartFormData`

``` purescript
multipartFormData :: MimeType
```


#### `textCSV`

``` purescript
textCSV :: MimeType
```


#### `textPlain`

``` purescript
textPlain :: MimeType
```


#### `textXML`

``` purescript
textXML :: MimeType
```




