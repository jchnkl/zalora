```
    # Look at Config.hs for configurable items like the port

    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal configure
    $ cabal build
    $ dist/build/zalora/zalora

    # Running the web service
    $ runghc -- -package-db --ghc-arg=.cabal-sandbox/x86_64-linux-ghc-7.8.2-packages.conf.d/ Main.hs

    # POSTing new items
    $ curl -H "Content-Type: application/json" -d "$(<   red.json)" http://localhost:8080/
    $ curl -H "Content-Type: application/json" -d "$(< green.json)" http://localhost:8080/
    $ curl -H "Content-Type: application/json" -d "$(<  blue.json)" http://localhost:8080/

    # Getting an item
    $ curl http://localhost:8080/item/${key}

    # Getting a list of items
    $ curl http://localhost:8080/items
```



Task
===

Write a FastCGI or HTTP server in Haskell that provides a restful API for managing an inventory of shoes:

* `POST` new shoes as a JSON body, with attributes "description", "color", "size",
and "photo". The "photo" attribute should be a base-64 encoded string
representing a JPEG image (think "data URI"). For example,

```
    { "description": "SADIE Faux Suede Heels with Bow"
    , "color": "red"
    , "size": "35"
    , "photo": "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgMCAgMDAwMEAwMEBQgFBQQEBQoHBwYIDAoMDAsKCwsNDhIQDQ4RDgsLEBYQERMUFRUVDA8XGBYUGBIUFRT/2wBDAQMEBAUEBQkFBQkUDQsNFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBT/wAARCAA2ACUDASIAAhEBAxEB/8QAGwABAQACAwEAAAAAAAAAAAAAAAkGBwEFCAr/xAAxEAABAwMDAgUCBAcAAAAAAAABAgMEBQYRAAchCBIJEyIxUTJhF0GCkSMzQlJxgaH/xAAaAQABBQEAAAAAAAAAAAAAAAAAAgMEBQcG/8QAKxEAAQMDAgMHBQAAAAAAAAAAAQACAwQFERIhIjFxBhMyQVFhgaHB0eHx/9oADAMBAAIRAxEAPwCqClJQMqISPknXPGpgeKRvTNuC7U7a0+oPRaVTGG35jbKyA9KWAtPdj3CElGPglWs18N/ruO4UWHtTuJPIuyKnyaRVJCuai2kfyXFH3eSBwT9YGD6h6mhICcLpKqxVFLRw1bjnvBqx6D+YPQ+xVCuPjTj40xpjTq5tONNMaaEKSviRbJ1qxt3pd5+WqTb1xL8xmTyfKfCfWyo/keO5Pyk8fSceHno1RolYj1ykyHYEyM+mTGlMEpU08ghQUlQ9iDj/AJq3XiFW8mv9LtwKUgKXCmQZCCRkpJkttEj9LqtS3bsuVZN6XFt7c7PlS4z6h5CvoWsDlSPstHaoEe4xqvk4HEBbXaqxl5tbYp/HHt8AAA/UZ/aq10T9TUfqe2Zh1qQW2rpppEGuREcdr4HDoH5IcHqH5A9w/p1v/wDfUH+nPqCqnRrvy7Ukoen27KIi1enoIzJjE5S4gHjzEZ7h+pOQFHVtLD3YtHcyzIV123X4VSoMsJDctDwSErUQA2sHBQvJAKFYIJxjUuN+oLL7pQOo5yGjhPL8LLP300wTpp1Ui6K+bHo249qVG27hhmfR56AiRHDq2ioBQUMLQQpJCkgggg8anB4ou1kml3dQL8hxVR1ONCDKlMjAUpHLKyf7u3KT9ko1T3H21je4W3dB3StSbblyQG6jSpiO1xpfuPggjkEHkEcjSHsDhhXNpuLrZVsn5tGxHqDz+x6gL5/Kg63e1WpzNecDDaXAl2ayj+IGyecge/8Aof4B9tbAvna/8MaYmo27PcqNjTXEOImJX6mncHsRISDjux3dqx6VDPbg9yR7wvjwmLSqSlu2zdtWpKlZwxMSiQ2PsOEq/cnWvZvhLXfPhsQHNzWDT2HFOtsrgLUkKPurt80DP31BMMgIwdlpMfaW2RytqYTpd5gt8uozg/K9PeH1uBeO4uxDU+7JaaoyxKMal1IvIcekR0oTkOkKKu9CypHrCVYSD6shRa7/AKP+lxXSxZVWoi7nkXM7UpglrUqMIzTJCAnCG+9fJxyrPOE8DHLU9uQBlZhc5Yp6yWWHGlxyMDA39lvzA0wNNNKVYnGuONNNCEOmmmhC/9k="
    }
```

* `GET` a shoe as an HTML page listing the shoe details, where the photo is served
as an `<img>` tag with "src" pointing to a path on the local filesystem (i.e. the
photo must be accessible as a local file, not as a data URI).

* `GET` a list of shoes as an HTML page with hyperlinks to all available shoes.



Feedback
===

#### Pros

- Includes README

- Returns valid JSON with "id" when POSTing shoes, e.g.
{"key":"7adb83aad0f0a74321696661ba96f6a17afb51aa"}

- It is not possible to access the database or other files on the disk via HTTP :)

- The data type for shoes (`Item`) is parametric over the image

#### Neutral

- Uses HDBC

- README says

curl -H "Content-Type: application/json" -d "$(< green.json)" http://localhost:8080/

instead of

curl -H "Content-Type: application/json" -d @green.json http://localhost:8080/

- Cabal file contains an seemingly arbitrary list of `other-extensions` for no
obvious reason

- Cabal file contains several unused (transitive?) dependencies. Is this an
attempt to simulate `cabal freeze`?

- FromJSON instance is defined by hand, not derived (GHC generics).

#### Cons

- No automated tests

- Returns 200 instead of 201 on POSTing shoe

- Ids for shoes look like hashes, but they have different length?!

(I think the issue here is that `showHex` is used to convert `Word8` into
`ShowS`, if you used with e.g. numbers < 16, it's not zero padded..)


- Accessing `/` results in 404, but returned JSON says `400`
 > GET / HTTP/1.1
 > User-Agent: curl/7.35.0
 > Host: localhost:4040
 > Accept: */*
 >
< HTTP/1.1 404 Not Found
< Transfer-Encoding: chunked
< Date: Wed, 13 Aug 2014 07:17:08 GMT
* Server Warp/3.0.0.7 is not blacklisted
< Server: Warp/3.0.0.7
< Content-Type: application/json
<
* Connection #0 to host localhost left intact
{"status":400,"exception":"service not found","message":"client error"}

- No valid HTML when GETting list of shoes:

$ curl http://localhost:4040/items
7adb83aad0f0a74321696661ba96f6a17afb51aa
<item/7adb83aad0f0a74321696661ba96f6a17afb51aa>


(same is true for shoe details page)

- Accesing `/item/` results in `Prelude.head: empty list` / server closing the
connection..

- When accessing the list of shoes through `/items/` instead of `/item` the
links to the shoes do not work, because he uses relative links.

- POSTing `blue.json` twice removes the image for the existing shoe and results in
{"status":500,"exception":"SqlError {seState = "", seNativeError = 19,
seErrorMsg = "step: UNIQUE constraint failed: items.key"}","message":"internal
server error"}

Furthermore the returned HTTP status code is 200 instead 500 (we will fix
this in `scotty`, https://github.com/scotty-web/scotty/pull/111).

- Accepts POSTed shoe with photo that is not valid Base64
