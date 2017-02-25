# This app will test elm-trello integrations

**In this REPO I will create a basic app trying to predict some interactions one will need to integrate an ELM APP with a Trello Board.

**Ok, but why the hell are you doing it?

**There are many use cases for this. For example, what if an app wants to use a trello board as a LOW LEVEL management resource and an ELM APP as a HIGH LEVEL management resoure. For example, in planning, one can create a big task in elm and create subtasks in trello. It would be useful if, and only if, we can automate stuff.

**Task list**
- [ ] Create Basic interface for showing cards data (maybe as cards also)
- [ ] Reading and list all trello cards from a given board
- [ ] Show card - with detailed information
- [ ] Show card - attachments
- [ ] Show card - custom field powerup
- [ ] Filter cards by column - like in trello
- [ ] Filter cards by tag
- [ ] Create cards in elm directly to the trello board
- [ ] Cards sync 1 - save cards actual state in database
- [ ] Cards sync 2 - detect updated cards and create action to update some or all cards (app database)
- [ ] Cards sync 3 - detect deleted cards and create action to delete (local database) or restore
- [ ] Create use case 1 - Create a task in Elm and decompose it as trello cards (also saved in local database)
- [ ] Create use case 2 - Combine attributes from trello subtasks into elm task



**But, first things first. How can you use and see whats happening here?**

- clone this repo (and install elm if not installed).
- npm install, when asked for sematic ui choose "Skip Install", and when asked "Where should we put Semantic UI inside your project?": dist/
- elm-package install
- Create firebase account and get firebase codes. To have this you will need to go to the firebase page, create a new project, enter the project and click "Add firebase to your web app" (or something like that), and then go to 5.

- create a firebaseConfig.js file dist/ folder. In this file you will need to add something like this:

```
'use strict';

// Initialize Firebase
var config = {
  apiKey: "apiKey",
  authDomain: "app_name.firebaseapp.com",
  databaseURL: "https://app_name.firebaseio.com",
  storageBucket: "app_name.appspot.com",
  messagingSenderId: "senderId"
};
```

- gulp (default task does everything one needs)
- Open chrome and go to localhost:4000
