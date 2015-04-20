# Module Documentation

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




