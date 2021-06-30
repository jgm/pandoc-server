# pandoc-server

A small example of a servant-based server for doing pandoc conversions.
Customize to fit your needs!

To install:

```
% stack install
```

To run the server:

```
% pandoc-server
```

This makes the server available at localhost port 8080.

There is one endpoint, `/convert`.  It expects a GET request
with a JSON request body containing the following fields:
`text` (text to convert),
`from` (defaults to `"markdown"`),
`to` (defaults to `"html"`),
`wrapText` (`"WrapAuto"` (default), `"WrapNone"`, or `"WrapPreserve"`),
`columns` (integer, defaults to 72).

Example of use (with `httpie`):

```
% http GET http://localhost:8080/convert text=:+1: from=markdown+emoji to=mediawiki Accept:text/plain
HTTP/1.1 200 OK
Content-Type: text/plain;charset=utf-8
Date: Wed, 30 Jun 2021 15:51:15 GMT
Server: Warp/3.3.16
Transfer-Encoding: chunked

<span class="emoji" data-emoji="+1">👍</span>
```

