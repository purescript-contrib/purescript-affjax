# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#171 by @JordanMartinez)
- Update all request functions to take a driver arg (#171 by @JordanMartinez)

  Affjax works on the Node.js and browser environments by relying on a `require`
  statement within a function. Depending on the environment detected,
  either `XHR` or `XmlHttpRequest` is used. Since ES modules do not allow
  one to call `import` within a function in a _synchronous_ way,
  we cannot continue to use this approach.

  Rather, all request-related functions (e.g. `request`, `get`, etc.) now take
  as their first argument an `AffjaxDriver` value. Different environments
  will pass in their implementation for that driver and re-export
  the functionality defined in `affjax`.

  To fix your code, depend on the corresponding library below and update the imported
  module from `Affjax` to `Affjax.Node`/`Affjax.Web`:
  - If on Node.js, use [`purescript-affjax-node`](https://github.com/purescript-contrib/purescript-affjax-node/).
  - If on the brower, use [`purescript-affjax-web`](https://github.com/purescript-contrib/purescript-affjax-web/).

New features:

Bugfixes:

Other improvements:
- Added `purs-tidy` formatter (#167 by @thomashoneyman)

## [v12.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v12.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#158)
- `XHRError Exn.Error` was removed and split to `TimeoutError`, `RequestFailedError`, and `XHROtherError Exn.Error` (#155, @srghma)

New features:

Bugfixes:

Other improvements:
- `XMLDocument` and `HTMLDocument` are now understood as `Document` responses (#157)
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#153)

## [v11.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v11.0.0) - 2020-09-06

- Added support for the `timeout` option in `Request` (#151)

## [v10.1.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v10.1.0) - 2020-06-07

- Added `Newtype` instance for `StatusCode` (@ford-prefect)

## [v10.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v10.0.0) - 2019-11-03

- Updated for latest `purescript-form-urlencoded`.
- Some helper functions were combined to accept `Maybe RequestBody` rather than having two variations of each.
- All request functions now return `Either Error _`; the `Aff` error channel is no longer used to capture errors from the `XHR` object, and the provided `Error` type captures the various possible error cases that can occur.
- `retry` was removed.

## [v9.0.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v9.0.1) - 2019-09-05

- Ensured version mismatch with `purescript-form-urlencoded` does not compile, potentially resulting in runtime errors (spotted by @menelaos)
- Don't override `nodejsBaseUrl` already set in `xhr2` on node (@paul-rouse)

## [v9.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v9.0.0) - 2019-03-11

- Updated for latest `purescript-argonaut-core`

## [v8.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v8.0.0) - 2019-02-23

- Updated `purescript-web-xhr` dependency
- Renamed functions in the `RequestHeader` / `ResponseHeader` modules to no longer prefix values with the module name

## [v7.0.2](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v7.0.2) - 2019-02-09

- XHR timeouts are now caught and raised through `Aff`'s error channel (@acple)

## [v7.0.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v7.0.1) - 2019-01-31

- Fixed an incorrect `request` example in the doc comments (@jhrcek)

## [v7.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v7.0.0) - 2018-08-12

Lots of things were renamed in this release to better match their current functionality:

- The `affjax` function is now `request`
- The `AffjaxRequest` record is now `Request`
- The `AffjaxResponse` record is now `Response`
- The `response` field of `Response` is now `body`
- The `Request` type is now `RequestBody`
- The `Response` type is now `ResponseFormat`
- The `Request` record now has a `responseFormat` field, rather than it being a separate argument to `request`

(Most of the new names by @paldepind).

Additionally, errors that occur during decoding a response no longer cause an entire request to fail. The `body` value is now `Either ResponseFormatError a`, so that other details about the response can still be inspected (status code, for example), even if the response body fails to decode. The `ResponseFormatError` value also carries the response body as a `Foreign` value, in case there is some further special handling that can be performed externally.

## [v6.0.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v6.0.1) - 2018-08-05

- Improved documentation (@paldepind)

## [v6.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v6.0.0) - 2018-05-27

- Updated for PureScript 0.12
- `Requestable` and `Respondable` classes have now been removed in favour of using explicit values
- `statusText` is now available in responses (@danbornside)
- The `Aff` canceller was fixed (@doolse)

## [v5.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v5.0.0) - 2017-09-14

- Updated for `purescript-aff` v4.0.0.

## [v4.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v4.0.0) - 2017-04-10

- Updated for PureScript 0.11 (@NasalMusician)

## [v3.0.2](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v3.0.2) - 2016-12-01

- Tweaked JS to enable `affjax` to work in Electron apps (@aratama)

## [v3.0.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v3.0.1) - 2016-11-15

- Fixed shadowed name warning

## [v3.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v3.0.0) - 2016-11-01

- Updated dependencies for PureScript 0.10

## [v2.0.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v2.0.1) - 2016-09-11

- Fixed a bug in splitting headers #81 (@jasonzoladz)

## [v2.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v2.0.0) - 2016-07-31

- Updated dependencies

## [v1.2.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v1.2.0) - 2016-07-28

- Added convenience functions for `PATCH` operations (@clayrat)

## [v1.0.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v1.0.0) - 2016-06-18

- Updated for the 1.0 core libraries and PureScript 0.9.x.

## [v0.13.2](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.13.2) - 2016-05-12

- Updated for Pursuit publishing

## [v0.13.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.13.1) - 2016-04-10

- Set `purescript-dom` lower bound to ensure compatibility with psc 0.8.4 (@damncabbage)

## [v0.13.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.13.0) - 2016-03-12

- Updated `purescript-aff` dependency
- Now uses types from `purescript-http-methods` and `purescript-media-types`

## [v0.12.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.12.0) - 2016-02-26

- Added `withCredentials` option (@brendanhay)

## [v0.11.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.11.0) - 2016-02-22

- Updated `purescript-aff` dependency

## [v0.10.4](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.10.4) - 2016-02-12

- Added `Requestable` instance for `FormURLEncoded` (@zudov)

## [v0.10.2](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.10.2) - 2016-01-30

- Errors rasied by `setRequestHeader` are now caught #58 (@codedmart)

## [v0.10.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.10.1) - 2016-01-07

- Fixed handling of JSON-value responses #54, #55

## [v0.10.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.10.0) - 2015-11-20

- Added `Json` instance for `Requestable` and `Respondable`
- `Requestable` and `Respondable` now include an optional `MimeType` to use as the default `Content-Type` or `Accept` header.

## [v0.9.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.9.0) - 2015-10-16

- Node support is now provided by `xhr2` rather than `xmlhttprequest` (fixes #44) (@stkb)

## [v0.8.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.8.1) - 2015-10-10

- Added `Respondable ArrayBuffer` instance (@quephird)

## [v0.8.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.8.0) - 2015-09-23

- Updated dependencies

## [v0.7.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.7.0) - 2015-08-31

- Updated dependencies and fixed warnings for PureScript 0.7.4

## [v0.5.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.5.1) - 2015-08-05

- Added support for node via npm package 'xmlhttprequest' (@hdgarrood)

## [v0.4.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.4.0) - 2015-07-07

- Updated for PureScript 0.7 (@qxjit)

## [v0.3.2](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.3.2) - 2015-06-08

- A better fix for `JSONResponse` use in `Respondable` - this fix preserves the original behaviour in custom `Respondable` instances.

## [v0.3.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.3.1) - 2015-06-08

- Fixed an issue with the `Respondable Foreign` instance - IE does not support automatic reading of JSON with `responseType = "json"`.

## [v0.3.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.3.0) - 2015-05-18

- Bumped `purescript-integers` dependency

## [v0.2.1](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.2.1) - 2015-04-23

- `ResponseType` constructors are now exported so that custom `Respondable` instances can be declared.

## [v0.2.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.2.0) - 2015-04-20

- Eliminates use of Proxy for a simpler implementation of Requestable.

## [v0.1.0](https://github.com/purescript-contrib/purescript-affjax/releases/tag/v0.1.0) - 2015-04-20

- This is the first beta release of Affjax, a library designed to expose AJAX through Aff.
