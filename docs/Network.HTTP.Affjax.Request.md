## Module Network.HTTP.Affjax.Request

#### `RequestContent`

``` purescript
data RequestContent :: *
```

Type representing all content types that be sent via XHR (ArrayBufferView,
Blob, Document, String, FormData).

##### Instances
``` purescript
instance requestableRequestContent :: Requestable RequestContent
```

#### `Requestable`

``` purescript
class Requestable a where
  toRequest :: a -> RequestContent
```

A class for types that can be converted to values that can be sent with
XHR requests.

##### Instances
``` purescript
instance requestableRequestContent :: Requestable RequestContent
instance requestableInt8Array :: Requestable (ArrayView Int8)
instance requestableInt16Array :: Requestable (ArrayView Int16)
instance requestableInt32Array :: Requestable (ArrayView Int32)
instance requestableUint8Array :: Requestable (ArrayView Uint8)
instance requestableUint16Array :: Requestable (ArrayView Uint16)
instance requestableUint32Array :: Requestable (ArrayView Uint32)
instance requestableUint8ClampedArray :: Requestable (ArrayView Uint8Clamped)
instance requestableFloat32Array :: Requestable (ArrayView Float32)
instance requestableFloat64Array :: Requestable (ArrayView Float64)
instance requestableBlob :: Requestable Blob
instance requestableDocument :: Requestable Document
instance requestableString :: Requestable String
instance requestableFormData :: Requestable FormData
instance requestableUnit :: Requestable Unit
```


