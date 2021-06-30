# pandoc-server

A small example of a servant-based server for doing pandoc conversions.
Customize to fit your needs!

## Installation

You will need [`stack`](https://docs.haskellstack.org/en/stable/README/)
or `cabal`. With `stack`:

```
% stack install
```

The executable will be put in `~/.local/bin`, so make sure this
is in your path.

## Running the server

```
% pandoc-server
```

This makes the server available at localhost port 8080.

## Using the server

There is one endpoint, `/convert`.  It expects a POST request
with a JSON body containing the following fields:

- `text` (text to convert)
- `from` (defaults to `"markdown"`)
- `to` (defaults to `"html"`)
- `wrapText` (`"WrapAuto"` (default), `"WrapNone"`, or `"WrapPreserve"`)
- `columns` (integer, defaults to 72)

Example of use (with `httpie`):

```
% http POST http://localhost:8080/convert text=:+1: from=markdown+emoji to=mediawiki Accept:text/plain
HTTP/1.1 200 OK
Content-Type: text/plain;charset=utf-8
Date: Wed, 30 Jun 2021 15:51:15 GMT
Server: Warp/3.3.16
Transfer-Encoding: chunked

<span class="emoji" data-emoji="+1">👍</span>
```

Performance, measured with `ab` on a 2.3 GHz i9 MacBook Pro:

- For 10,000 sequential conversions of a very short
  text from markdown to latex, 2852 requests per second.

- With 10 parallel threads making requests, 4081 requests per
  second.

