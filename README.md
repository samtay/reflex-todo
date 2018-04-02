# reflex-todo [![Build Status](https://travis-ci.org/samtay/reflex-todo.svg?branch=heroku)](https://travis-ci.org/samtay/reflex-todo)

This codebase aims to be an example for a full stack websocket-driven [reflex
project](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md)
of sufficient complexity. By sufficient, I mean that this should provide a
better idea of how to start building a real-world application, of greater
complexity than, say, a "Hello World" demonstration.

First, read the [reflex
project](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md)
document (it's not long). A great starting point after reading that document
would be ElvishJerricco's [reflex-project-skeleton](https://github.com/ElvishJerricco/reflex-project-skeleton), which is what this
codebase started from.

### things of interest
1. Some nice development scripts in [bin](./bin), for those of us less familiar
   with nix.
2. A very hacky way to deploy this style of project to
   [heroku](https://github.com/samtay/reflex-todo/tree/heroku) (forgoing nix
   and leveraging tools built around stack).
3. Complete configuration of Android and iOS apps, and scripts for deploying
   them.
4. Slides from presenting this application (coming soon).

### todo
1. Add iOS app
2. Add bin/ scripts for android, ios, backend, etc.
2. Perhaps capture slides of some backend / common code
4. Show new frontend app code
5. Try adding a new feature, such as editing text
6. Add active user count
7. Add note about Requester typeclass and a GADT api

### scaling
Some things will obviously change when trying to build an app that really
scales. I remembered reading about
[acid-state](https://github.com/acid-state/acid-state) in the [24 Days of
Hackage](https://ocharles.org.uk/blog/posts/2013-12-14-24-days-of-hackage-acid-state.html)
article so I chose to try that out here, and it fits the bill very well for
this todo list.  However, most people will probably reach for a more familiar
database such as postgres. If postgres were being used here, there wouldn't be
too much of a code change, but one thing would be cleaned up: *broadcasting*.

At work, we have an architecture using reflex, websockets, and postgres, and we
leverage postgres NOTIFY/LISTEN so that whenever updates are made to the
database we can parse the notification and propagate the new state directly to
all the frontend clients. This way, each websocket connection doesn't need to
worry about communicating with all the other connections, but we still maintain a very
high level of interactivity.

### Sources
1. [reflex-platform](https://github.com/reflex-frp/reflex-platform)
2. [reflex-project-skeleton](https://github.com/ElvishJerricco/reflex-project-skeleton)
2. [Playin with WebSockets in Haskell and Elm](https://www.paramander.com/blog/playing-with-websockets-in-haskell-and-elm)
3. [24 Days of Hackage: acid-state](https://ocharles.org.uk/blog/posts/2013-12-14-24-days-of-hackage-acid-state.html)
