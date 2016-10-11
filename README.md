# servant-yaml

> Servant support for yaml

[![Build Status](https://travis-ci.org/phadej/servant-yaml.svg?branch=master)](https://travis-ci.org/phadej/servant-yaml)
[![Hackage](https://img.shields.io/hackage/v/servant-yaml.svg)](http://hackage.haskell.org/package/servant-yaml)
[![Stackage LTS 2](http://stackage.org/package/servant-yaml/badge/lts-2)](http://stackage.org/lts-2/package/servant-yaml)
[![Stackage LTS 3](http://stackage.org/package/servant-yaml/badge/lts-3)](http://stackage.org/lts-3/package/servant-yaml)
[![Stackage Nightly](http://stackage.org/package/servant-yaml/badge/nightly)](http://stackage.org/nightly/package/servant-yaml)

## Example

Check [`example/Main.hs`](https://github.com/phadej/servant-yaml/blob/master/example/Main.hs) for an example:

```
curl -i -H "Content-Type: application/x-yaml" -H "Accept: application/x-yaml" -X POST --data-binary @example.yaml 'localhost:8000/foo'

$ curl -i localhost:8000
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Sun, 01 Nov 2015 08:10:01 GMT
Server: Warp/3.0.13.1
Content-Type: application/x-yaml

foo: 42
bar: Yaml!

$ curl -i -H "Accept: application/json" localhost:8000
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Sun, 01 Nov 2015 08:14:08 GMT
Server: Warp/3.0.13.1
Content-Type: application/json

{"foo":42,"bar":"Yaml!"

$ cat example.yaml
bar: "JSON?"
foo: 1337

$ curl -i -H "Content-Type: application/x-yaml" -H "Accept: application/x-yaml" -X POST --data-binary @example.yaml 'localhost:8000/foo'
HTTP/1.1 201 Created
Transfer-Encoding: chunked
Date: Sun, 01 Nov 2015 08:15:21 GMT
Server: Warp/3.0.13.1
Content-Type: application/x-yaml

foo: 1337
bar: JSON?

$ curl -H 'Accept: text/yaml' -D - http://localhost:8000/
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Tue, 11 Oct 2016 05:53:29 GMT
Server: Warp/3.2.8
Content-Type: text/yaml

foo: 42
bar: Yaml!
```
