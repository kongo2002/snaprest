
# snaprest

This is a small RESTful web service implemented in Haskell using the
[snap][snap] framework and using a [MongoDB][mongo] backend.


## Installation

As prerequisites you will have to install `mongodb` and `ghc`. Follow your OS's
and distribution's installation intructions for that. After that you can
continue with installing *snaprest* via `cabal`:

    $ git clone git://github.com/kongo2002/snaprest.git
    $ cd snaprest
    $ cabal install

Now you should be able to start *snaprest* on your console. Make sure your
compiled executable can be found in your `PATH` and your `mongodb` is running.

    $ snaprest


## Testing

Once *snaprest* is running you should be able to test the web service using
`curl`:

~~~
$ curl -X PUT -d '{"firstName":"some","lastName":"one"}' localhost:8000/users/user

{"data":{"id":1,"firstName":"some","lastName":"one"},"success":true}

$ curl localhost:8000/users/user/1

{"data":{"id":1,"firstName":"some","lastName":"one"},"success":true}
~~~


## Development

During development it is really handy to be able to test *snaprest* in your
*GHCI* interactive haskell shell:

~~~ .haskell
Prelude> :l Main
...
*Main> main
...
Listening on http://0.0.0.0:8000/
~~~

[snap]: http://snapframework.com/
[mongo]: http://www.mongodb.org/
