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

You can construct requests with the `affjax` function:

```purescript
module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (affjax, defaultRequest)

main = launchAff $ do
  res <- affjax $ defaultRequest { url = "/api", method = Left GET }
  liftEff $ log $ "GET /api response: " <> res.response
```

(`defaultRequest` is a record value that has all the required fields pre-set for convenient overriding when making a request.)

Or use of a number of helpers for common cases:

```purescript
import Network.HTTP.Affjax (get, post)

main = launchAff $ do
  res1 <- get "/api"
  liftEff $ log $ "GET /api response: " <> res1.response

  res2 <- post "/api" someData
  liftEff $ log $ "POST /api response: " <> res2.response
```

See the module documentation for a full list of these helpers.

When sending data in a request the Requestable class enables automatic conversion into a format that is acceptable for an XHR request. Correspondingly there is a Respondable class for reading data that comes back from the server.

## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-affjax).
