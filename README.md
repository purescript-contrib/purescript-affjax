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

main = void $ launchAff $ do
  result <- AX.request (AX.defaultRequest { url = "/api", method = Left GET, responseFormat = ResponseFormat.json })
  case result of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> log $ "GET /api response: " <> J.stringify response.body
```

(`defaultRequest` is a record value that has all the required fields pre-set for convenient overriding when making a request.)

There are also a number of helpers for common `get`, `post`, `put`, `delete`, and `patch` cases:

```purescript
import Affjax.RequestBody as RequestBody
import Data.Maybe (Maybe(..))

main = void $ launchAff $ do
  result1 <- AX.get ResponseFormat.json "/api"
  case result1 of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> log $ "GET /api response: " <> J.stringify response.body

  result2 <- AX.post ResponseFormat.json "/api" (Just (RequestBody.json (J.fromString "test")))
  case result2 of
    Left err -> log $ "POST /api response failed to decode: " <> AX.printError err
    Right response -> log $ "POST /api response: " <> J.stringify response.body
```

See the [main module documentation](https://pursuit.purescript.org/packages/purescript-affjax/docs/Affjax) for a full list of these helpers and their variations.

## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-affjax).
