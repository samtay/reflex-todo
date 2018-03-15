# todo

1. Override stm to latest
2. Perhaps capture slides of some backend / common code
3. Show some of the new glue on the frontend
4. Show new frontend app code
5. Try adding a new feature, such as editing text

6. travis / circleci
7. heroku
8. automate all the things

### Notes
If someone is looking at this as an example of how they might set up an
application with more complexity, keep a few things in mind when reading this
codebase.

#### Data Storage
I selfishly chose to try out
[acid-state](https://github.com/acid-state/acid-state) for persistent data,
because I had heard good things and was curious. Apparently it actually can
scale quite well, but most people are probably reaching for a more familiar
database such as postgres. If postgres were being used here, there wouldn't be
too much of a code change, but one thing would be cleaned up: *broadcasting*.

Currently the backend application code here really has nothing going on in its
"central" thread other than starting up the server. Everything else happens in
the forked "client" threads which write to a broadcast channel and read from a
listener channel. To me, this is a bit awkward for two reasons: each one
listens to its own broadcast, and each one has very direct power to affect the
sibling client threads.

At work, we have an architecture using reflex, websockets, and postgres, and we
leverage postgres NOTIFY/LISTEN so that whenever updates are made to the
database we can parse the notification and propagate the new state directly to
all the frontend clients. This way, each websocket connection doesn't need to
worry about affecting all the other connections, but we still maintain a very
high level of interactivity.

### Sources
1. [reflex-platform](https://github.com/reflex-frp/reflex-platform)
2. [reflex-project-skeleton](https://github.com/ElvishJerricco/reflex-project-skeleton)
2. [Playin with WebSockets in Haskell and Elm](https://www.paramander.com/blog/playing-with-websockets-in-haskell-and-elm)
3. [24 Days of Hackage: acid-state](https://ocharles.org.uk/blog/posts/2013-12-14-24-days-of-hackage-acid-state.html)
