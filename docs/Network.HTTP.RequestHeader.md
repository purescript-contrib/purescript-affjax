## Module Network.HTTP.RequestHeader

#### `RequestHeader`

``` purescript
data RequestHeader
  = Accept MimeType
  | ContentType MimeType
  | RequestHeader String String
```

##### Instances
``` purescript
instance eqRequestHeader :: Eq RequestHeader
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


