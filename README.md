# purescript-affjax

[![Latest release](http://img.shields.io/github/release/slamdata/purescript-affjax.svg)](https://github.com/slamdata/purescript-affjax/releases)
[![Build status](https://travis-ci.org/slamdata/purescript-affjax.svg?branch=master)](https://travis-ci.org/slamdata/purescript-affjax)

A library taking advantage of [`purescript-aff`](https://github.com/slamdata/purescript-aff) to enable pain-free asynchronous AJAX requests and response handling.

# Getting Started

## Installation

```
bower install purescript-affjax
```

If you are intending to use the library in a Node.js setting rather than the browser, you will need an additional dependency from `npm`:

```
npm install xhr2
```

## Introduction

You can construct requests with the `request` function:

```purescript
module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (launchAff)
import Effect.Class.Console (log)

main = launchAff $ do
  res <- AX.request (AX.defaultRequest { url = "/api", method = Left GET, responseFormat = ResponseFormat.json })
  case res.body of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printResponseFormatError err
    Right json -> log $ "GET /api response: " <> J.stringify json
```

(`defaultRequest` is a record value that has all the required fields pre-set for convenient overriding when making a request.)

There are also a number of helpers for common `get`, `post`, `put`, `delete`, and `patch` cases:

```purescript
import Affjax.RequestBody as RequestBody

main = launchAff $ do
  res1 <- AX.get ResponseFormat.json "/api"
  case res1.body of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printResponseFormatError err
    Right json -> log $ "GET /api response: " <> J.stringify json

  res2 <- AX.post ResponseFormat.json "/api" (RequestBody.json (J.fromString "test"))
  case res2.body of
    Left err -> log $ "POST /api response failed to decode: " <> AX.printResponseFormatError err
    Right json -> log $ "POST /api response: " <> J.stringify json
```

See the [main module documentation](https://pursuit.purescript.org/packages/purescript-affjax/docs/Affjax) for a full list of these helpers and their variations.

## Error handling

There are two ways an Affjax request can fail: there's either some problem with the request itself, or the result that comes back is not as expected.

For the first case, these errors will be things like the URL being invalid or the server not existing, and will occur in the `Aff` error channel. The [`try`](https://pursuit.purescript.org/packages/purescript-aff/docs/Effect.Aff#v:try) function can lift these errors out of the error channel so the result of a request becomes `Aff (Either Error (Response _))`.

The latter case occurs when we did get a response for the request, but the result that came back could not be handled in the way that was expected. In these situations the `body` value of the `Response` will be a `Left` value with the error message describing what went wrong.

## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-affjax).
