# purescript-affjax

[![Build Status](https://travis-ci.org/slamdata/purescript-affjax.svg?branch=master)](https://travis-ci.org/slamdata/purescript-affjax)

A library taking advantage of [`purescript-aff`](https://github.com/slamdata/purescript-aff) to enable pain-free asynchronous AJAX requests and response handling.

# Getting Started

## Installation

```
bower install purescript-aff
```

## Introduction

You can construct requests with the `affjax` function:

```purescript
main = launchAff $ do
  res <- affjax $ defaultRequest { url = "/api", method = GET }
  liftEff $ log $ "GET /api response: " ++ res.response
```

(`defaultRequest` is a record value that has all the required fields pre-set for convenient overriding when making a request.)

Or use of a number of helpers for common cases:

```purescript
main = launchAff $ do
  res1 <- get "/api"
  liftEff $ log $ "GET /api response: " ++ res1.response

  res2 <- post "/api" someData
  liftEff $ log $ "POST /api response: " ++ res2.response
```

See the module documentation for a [full list of these helpers](docs/Network.HTTP.Affjax.md#get).

When sending data in a request the [`Requestable`](docs/Network.HTTP.Affjax.Request.md#requestable) class enables automatic conversion into a format that is acceptable for an XHR request. Correspondingly there is a [`Respondable`](docs/Network.HTTP.Affjax.Response.md#respondable) class for reading data that comes back from the server.

## Module documentation

- [Network.HTTP.Affjax](docs/Network.HTTP.Affjax.md)
- [Network.HTTP.Affjax.Request](docs/Network.HTTP.Affjax.Request.md)
- [Network.HTTP.Affjax.Response](docs/Network.HTTP.Affjax.Response.md)

### General HTTP

Note: these values and types will most likely be moved into separate libraries at a later date.

- [Network.HTTP.Method](docs/Network.HTTP.Method.md)
- [Network.HTTP.RequestHeader](docs/Network.HTTP.RequestHeader.md)
- [Network.HTTP.ResponseHeader](docs/Network.HTTP.ResponseHeader.md)
- [Network.HTTP.MimeType](docs/Network.HTTP.MimeType.md)
- [Network.HTTP.MimeType.Common](docs/Network.HTTP.MimeType.Common.md)
- [Network.HTTP.StatusCode](docs/Network.HTTP.StatusCode.md)
