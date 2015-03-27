# Module Documentation

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

#### `AffjaxOptions`

``` purescript
data AffjaxOptions :: *
```

Options type for Affjax requests.

#### `AffjaxResponse`

``` purescript
type AffjaxResponse a = { response :: a, headers :: [ResponseHeader], status :: StatusCode }
```

The type of records that will be received as an Affjax response.

#### `url`

``` purescript
url :: Option AffjaxOptions String
```

Sets the URL for a request.

#### `method`

``` purescript
method :: Option AffjaxOptions Method
```

Sets the HTTP method for a request.

#### `content`

``` purescript
content :: Option AffjaxOptions RequestContent
```

Sets the content to send in a request.

#### `headers`

``` purescript
headers :: Option AffjaxOptions [RequestHeader]
```

Sets the headers to send with a request.

#### `username`

``` purescript
username :: Option AffjaxOptions String
```

Sets the HTTP auth username to send with a request.

#### `password`

``` purescript
password :: Option AffjaxOptions String
```

Sets the HTTP auth password to send with a request.

#### `affjax`

``` purescript
affjax :: forall e a. Responsable a -> Options AffjaxOptions -> Affjax e a
```

Runs a request.

#### `affjax'`

``` purescript
affjax' :: forall e a. Responsable a -> Options AffjaxOptions -> (Error -> Eff (ajax :: Ajax | e) Unit) -> (AffjaxResponse a -> Eff (ajax :: Ajax | e) Unit) -> Eff (ajax :: Ajax | e) Unit
```

Runs a request directly in Eff.

#### `get`

``` purescript
get :: forall e a. Responsable a -> String -> Affjax e a
```


#### `post`

``` purescript
post :: forall e a. Responsable a -> String -> RequestContent -> Affjax e a
```


#### `post_`

``` purescript
post_ :: forall e. String -> RequestContent -> Affjax e Unit
```


#### `put`

``` purescript
put :: forall e a. Responsable a -> String -> RequestContent -> Affjax e a
```


#### `put_`

``` purescript
put_ :: forall e. String -> RequestContent -> Affjax e Unit
```


#### `delete`

``` purescript
delete :: forall e a. Responsable a -> String -> Affjax e a
```


#### `delete_`

``` purescript
delete_ :: forall e. String -> Affjax e Unit
```



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


#### `isOptionMethod`

``` purescript
instance isOptionMethod :: IsOption Method
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


#### `isOptionRequestHeader`

``` purescript
instance isOptionRequestHeader :: IsOption RequestHeader
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

#### `isOptionRequestContent`

``` purescript
instance isOptionRequestContent :: IsOption RequestContent
```


#### `Requestable`

``` purescript
class Requestable a where
  toContent :: a -> RequestContent
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
data Responsable a
  = Responsable (ResponseContent -> F a) ResponseType
```


#### `rInt8Array`

``` purescript
rInt8Array :: Responsable A.Int8Array
```


#### `rInt16Array`

``` purescript
rInt16Array :: Responsable A.Int16Array
```


#### `rInt32Array`

``` purescript
rInt32Array :: Responsable A.Int32Array
```


#### `rUint8Array`

``` purescript
rUint8Array :: Responsable A.Uint8Array
```


#### `rUint16Array`

``` purescript
rUint16Array :: Responsable A.Uint16Array
```


#### `rUint32Array`

``` purescript
rUint32Array :: Responsable A.Uint32Array
```


#### `rUint8ClampedArray`

``` purescript
rUint8ClampedArray :: Responsable A.Uint8ClampedArray
```


#### `rFloat32Array`

``` purescript
rFloat32Array :: Responsable A.Float32Array
```


#### `rFloat64Array`

``` purescript
rFloat64Array :: Responsable A.Float64Array
```


#### `rBlob`

``` purescript
rBlob :: Responsable Blob
```


#### `rDocument`

``` purescript
rDocument :: Responsable Document
```


#### `rJSON`

``` purescript
rJSON :: Responsable Foreign
```


#### `rString`

``` purescript
rString :: Responsable String
```


#### `rUnit`

``` purescript
rUnit :: Responsable Unit
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


#### `isOptionResponseType`

``` purescript
instance isOptionResponseType :: IsOption ResponseType
```


#### `ajaxResponseTypeToString`

``` purescript
ajaxResponseTypeToString :: ResponseType -> String
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




