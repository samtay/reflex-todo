# reflex-todo [![Build Status](https://travis-ci.org/samtay/reflex-todo.svg?branch=heroku)](https://travis-ci.org/samtay/reflex-todo) [![Heroku](https://heroku-badge.herokuapp.com/?app=reflex-todo&svg=1)](https://reflex-todo.herokuapp.com/)

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

### todo
1. Add iOS app
2. Add bin/ scripts for android, ios, backend, etc.
2. Perhaps capture slides of some backend / common code
4. Show new frontend app code
5. Try adding a new feature, such as editing text
6. Add active user count
7. Add note about Requester typeclass and a GADT api (or just figure it out)

### things of interest
1. Development scripts in [bin](./bin), for those of us less familiar with nix.
2. Complete configuration of Android and iOS apps, and scripts for deploying
   them.
3. Slides from presenting this application (coming soon).

### building
To build the project to its various compilation targets, see `bin/build --help`. For example,
```shell
# build frontend web app
./bin/build frontend
ls dist-frontend/index.html

# build android app
./bin/build android
ls dist-android/android-app.apk

# build ios ad-hoc app
# TODO this will have more arguments
./bin/build --ad-hoc ios
ls dist-ios/ios-app.ipa

# deploy ios app to connected device
./bin/build ios
```

### hacking
To develop on top of this project, or a fork off it:
```shell
# get those sweet ghcid shells, for whichever parts of the codebase you are working on
./bin/ghcid frontend
./bin/ghcid backend
./bin/ghcid test

# start up the backend
./bin/ghci backend # main
# start up a warp frontend
./bin/ghci frontend # warp

# code & reload ghci modules as necessary
```

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
2. [Playing with WebSockets in Haskell and Elm](https://www.paramander.com/blog/playing-with-websockets-in-haskell-and-elm)
3. [24 Days of Hackage: acid-state](https://ocharles.org.uk/blog/posts/2013-12-14-24-days-of-hackage-acid-state.html)
4. [shell-helpers](https://github.com/briceburg/shell-helpers)
