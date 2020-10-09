# Affjax

[![CI](https://github.com/purescript-contrib/purescript-affjax/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-affjax/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-affjax.svg)](https://github.com/purescript-contrib/purescript-affjax/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-affjax/badge)](https://pursuit.purescript.org/packages/purescript-affjax)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-teal.svg)](https://github.com/thomashoneyman)

A library taking advantage of [`aff`](https://github.com/purescript-contrib/purescript-aff) to enable pain-free asynchronous AJAX requests and response handling.

## Installation

Install `affjax` with [Spago](https://github.com/purescript/spago):

```sh
spago install affjax
```

If you are using `affjax` in a Node.js setting you will also need to install an additional NPM dependency:

```
npm install xhr2
```

## Quick start

You can construct requests with the `request` function:

```purescript
module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (stringify, fromString)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (launchAff)
import Effect.Class.Console (log)

main = void $ launchAff $ do
  result <- AX.request (AX.defaultRequest { url = "/api", method = Left GET, responseFormat = ResponseFormat.json })
  case result of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> log $ "GET /api response: " <> stringify response.body
```

(`defaultRequest` is a record value that has all the required fields pre-set for convenient overriding when making a request.)

There are also a number of helpers for common `get`, `post`, `put`, `delete`, and `patch` cases:

```purescript
import Affjax.RequestBody as RequestBody
import Data.Maybe (Maybe(..))
import Effect.Aff (launchAff_)

main = launchAff_ do
  result1 <- AX.get ResponseFormat.json "/api"
  case result1 of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> log $ "GET /api response: " <> stringify response.body

  result2 <- AX.post ResponseFormat.json "/api" (Just (RequestBody.json (fromString "test")))
  case result2 of
    Left err -> log $ "POST /api response failed to decode: " <> AX.printError err
    Right response -> log $ "POST /api response: " <> stringify response.body
```

## Documentation

`affjax` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-affjax).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-affjax/issues) if you have encountered a bug or problem.
- [Search or start a thread on the PureScript Discourse](https://discourse.purescript.org) if you have general questions. You can also ask questions in the `#purescript` and `#purescript-beginners` channels on the [Functional Programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com/)).

## Contributing

You can contribute to `affjax` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-affjax/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
