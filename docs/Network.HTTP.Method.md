# Module Documentation

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




