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
Eq RequestHeader
Show RequestHeader
```

#### `requestHeaderName`

``` purescript
requestHeaderName :: RequestHeader -> String
```

#### `requestHeaderValue`

``` purescript
requestHeaderValue :: RequestHeader -> String
```


