# Module Documentation

## Module Type.Proxy

#### `Proxy`

``` purescript
data Proxy a
  = Proxy 
```



## Module Network.HTTP.Affjax

#### `Ajax`

``` purescript
data Ajax :: !
```

The effect type for AJAX requests made with Affjax.

#### `Affjax`

``` purescript
type Affjax e a = Aff (ajax :: Ajax | e) (AffjaxResponse a)
```

The type for Affjax requests.

#### `AffjaxRequest`

``` purescript
type AffjaxRequest a = { password :: Maybe String, username :: Maybe String, content :: Maybe a, headers :: [RequestHeader], url :: URL, method :: Method }
```


#### `defaultRequest`

``` purescript
defaultRequest :: AffjaxRequest Unit
```


#### `AffjaxResponse`

``` purescript
type AffjaxResponse a = { response :: a, headers :: [ResponseHeader], status :: StatusCode }
```

The type of records that will be received as an Affjax response.

#### `URL`

``` purescript
type URL = String
```

Type alias for URL strings to aid readability of types.

#### `affjax`

``` purescript
affjax :: forall e a b. (Requestable a, Responsable b) => AffjaxRequest a -> Affjax e b
```

Makes an `Affjax` request.

#### `get`

``` purescript
get :: forall e a. (Responsable a) => URL -> Affjax e a
```


#### `post`

``` purescript
post :: forall e a b. (Requestable a, Responsable b) => URL -> a -> Affjax e b
```


#### `post_`

``` purescript
post_ :: forall e a. (Requestable a) => URL -> a -> Affjax e Unit
```


#### `put`

``` purescript
put :: forall e a b. (Requestable a, Responsable b) => URL -> a -> Affjax e b
```


#### `put_`

``` purescript
put_ :: forall e a. (Requestable a) => URL -> a -> Affjax e Unit
```


#### `delete`

``` purescript
delete :: forall e a. (Responsable a) => URL -> Affjax e a
```


#### `delete_`

``` purescript
delete_ :: forall e. URL -> Affjax e Unit
```


#### `affjax'`

``` purescript
affjax' :: forall e a b. (Requestable a, Responsable b) => AffjaxRequest a -> (Error -> Eff (ajax :: Ajax | e) Unit) -> (AffjaxResponse b -> Eff (ajax :: Ajax | e) Unit) -> Eff (ajax :: Ajax | e) Unit
```

Run a request directly without using `Aff`.


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
  | MOVE 
  | COPY 
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
```


#### `responseHeader`

``` purescript
responseHeader :: String -> String -> ResponseHeader
```


#### `eqResponseHeader`

``` purescript
instance eqResponseHeader :: Eq ResponseHeader
```


#### `showResponseHeader`

``` purescript
instance showResponseHeader :: Show ResponseHeader
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



## Module Network.HTTP.Affjax.Request

#### `RequestContent`

``` purescript
data RequestContent :: *
```

Type representing all content types that be sent via XHR (ArrayBufferView,
Blob, Document, String, FormData).

#### `Requestable`

``` purescript
class Requestable a where
  toRequest :: a -> RequestContent
```

A class for types that can be converted to values that can be sent with
XHR requests.

#### `requestableRequestContent`

``` purescript
instance requestableRequestContent :: Requestable RequestContent
```


#### `requestableInt8Array`

``` purescript
instance requestableInt8Array :: Requestable (A.ArrayView A.Int8)
```


#### `requestableInt16Array`

``` purescript
instance requestableInt16Array :: Requestable (A.ArrayView A.Int16)
```


#### `requestableInt32Array`

``` purescript
instance requestableInt32Array :: Requestable (A.ArrayView A.Int32)
```


#### `requestableUint8Array`

``` purescript
instance requestableUint8Array :: Requestable (A.ArrayView A.Uint8)
```


#### `requestableUint16Array`

``` purescript
instance requestableUint16Array :: Requestable (A.ArrayView A.Uint16)
```


#### `requestableUint32Array`

``` purescript
instance requestableUint32Array :: Requestable (A.ArrayView A.Uint32)
```


#### `requestableUint8ClampedArray`

``` purescript
instance requestableUint8ClampedArray :: Requestable (A.ArrayView A.Uint8Clamped)
```


#### `requestableFloat32Array`

``` purescript
instance requestableFloat32Array :: Requestable (A.ArrayView A.Float32)
```


#### `requestableFloat64Array`

``` purescript
instance requestableFloat64Array :: Requestable (A.ArrayView A.Float64)
```


#### `requestableBlob`

``` purescript
instance requestableBlob :: Requestable Blob
```


#### `requestableDocument`

``` purescript
instance requestableDocument :: Requestable Document
```


#### `requestableString`

``` purescript
instance requestableString :: Requestable String
```


#### `requestableFormData`

``` purescript
instance requestableFormData :: Requestable FormData
```


#### `requestableUnit`

``` purescript
instance requestableUnit :: Requestable Unit
```



## Module Network.HTTP.Affjax.Response

#### `ResponseContent`

``` purescript
type ResponseContent = Foreign
```

Type representing content types that be received from an XHR request
(ArrayBuffer, Blob, Document, JSON, String).

#### `Responsable`

``` purescript
class Responsable a where
  responseType :: Proxy a -> ResponseType
  fromResponse :: ResponseContent -> F a
```


#### `responsableBlob`

``` purescript
instance responsableBlob :: Responsable Blob
```

#### `responsableDocument`

``` purescript
instance responsableDocument :: Responsable Document
```


#### `responsableJSON`

``` purescript
instance responsableJSON :: Responsable Foreign
```


#### `responsableString`

``` purescript
instance responsableString :: Responsable String
```


#### `responsableUnit`

``` purescript
instance responsableUnit :: Responsable Unit
```



## Module Network.HTTP.Affjax.ResponseType

#### `ResponseType`

``` purescript
data ResponseType
  = ArrayBufferResponse 
  | BlobResponse 
  | DocumentResponse 
  | JSONResponse 
  | StringResponse 
```

Valid response types for an AJAX request. This is used to determine the
`ResponseContent` type for a request.

#### `eqResponseType`

``` purescript
instance eqResponseType :: Eq ResponseType
```


#### `showResponseType`

``` purescript
instance showResponseType :: Show ResponseType
```


#### `responseTypeToString`

``` purescript
responseTypeToString :: ResponseType -> String
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


#### `textHTML`

``` purescript
textHTML :: MimeType
```


#### `textPlain`

``` purescript
textPlain :: MimeType
```


#### `textXML`

``` purescript
textXML :: MimeType
```




