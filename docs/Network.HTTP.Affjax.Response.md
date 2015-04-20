# Module Documentation

## Module Network.HTTP.Affjax.Response

#### `ResponseType`

``` purescript
data ResponseType a
```

Valid response types for an AJAX request. This is used to determine the
`ResponseContent` type for a request. The `a` type variable is a phantom
type used to associate the `ResponseType` with a particular instance of
`Respondable`.

#### `eqResponseType`

``` purescript
instance eqResponseType :: Eq (ResponseType a)
```


#### `showResponseType`

``` purescript
instance showResponseType :: Show (ResponseType a)
```


#### `responseTypeToString`

``` purescript
responseTypeToString :: forall a. ResponseType a -> String
```


#### `ResponseContent`

``` purescript
type ResponseContent = Foreign
```

Type representing content types that be received from an XHR request
(Blob, Document, JSON, String).

#### `Respondable`

``` purescript
class Respondable a where
  responseType :: ResponseType a
  fromResponse :: ResponseContent -> F a
```


#### `responsableBlob`

``` purescript
instance responsableBlob :: Respondable Blob
```


#### `responsableDocument`

``` purescript
instance responsableDocument :: Respondable Document
```


#### `responsableJSON`

``` purescript
instance responsableJSON :: Respondable Foreign
```


#### `responsableString`

``` purescript
instance responsableString :: Respondable String
```


#### `responsableUnit`

``` purescript
instance responsableUnit :: Respondable Unit
```




