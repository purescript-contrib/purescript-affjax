# Module Documentation

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




