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

##### Instances
``` purescript
Eq Method
Show Method
```

#### `methodToString`

``` purescript
methodToString :: Method -> String
```


