# Notes

I'm currently writing this app as a reflex demo. Once finished I will link the slides and this can be a reference for creating a dead simple full stack websocket-driven haskell application. Right now this is a WIP, preparing the code and talk.

### Play Around
Intro. Cool thing about this is compilation to different targets, all from a
single haskell codebase. Link web/ios/android store links (send to meetup first
as well). Show frontend/common/backend structure, explain the frontend compiles
to those different targets, backend is the server, and both are compiled with
the modules from common.

### Frontend
1. First make SIMPLE list with code fitting on one slide. Capture slide.
2. Next add + option, capture slide.
3. Next add - option, capture slide.
4. Next add edit option, capture slide.

### Backend
1. Next add web socket connection to make CRUD requests
2. Show backend slide accepting private requests and generating view/patches
3. Show frontend slide with dynamic view from backend
4. NOTE that someone might delete an item while another user is editing.
5. Add requests for starting/stopping an edit.
6. Propagate the edit status to the view
7. Mark the deletion disabled while item is edited.

### Consideration
1. Need brief description of reflex philosophy (events/behaviors/dynamics)
2. After frontend is done, save the code under Frontend.Static.app; if
   websocket connection fails, show an error message, but allow user to play
   with non-persistent data.
