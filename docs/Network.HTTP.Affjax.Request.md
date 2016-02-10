## Module Network.HTTP.Affjax.Request

#### `RequestContent`

``` purescript
data RequestContent :: *
```

Type representing all content types that be sent via XHR (ArrayBufferView,
Blob, Document, String, FormData).

##### Instances
``` purescript
Requestable RequestContent
```

#### `Requestable`

``` purescript
class Requestable a where
  toRequest :: a -> Tuple (Maybe MimeType) RequestContent
```

A class for types that can be converted to values that can be sent with
XHR requests. An optional mime-type can be specified for a default
`Content-Type` header.

##### Instances
``` purescript
Requestable RequestContent
Requestable (ArrayView Int8)
Requestable (ArrayView Int16)
Requestable (ArrayView Int32)
Requestable (ArrayView Uint8)
Requestable (ArrayView Uint16)
Requestable (ArrayView Uint32)
Requestable (ArrayView Uint8Clamped)
Requestable (ArrayView Float32)
Requestable (ArrayView Float64)
Requestable Blob
Requestable Document
Requestable String
Requestable Json
Requestable FormData
Requestable Unit
```


