PSC-GUI
---

This tool is meant to be a graphical frontend for
[psc-ide](https://github.com/kRITZCREEK/psc-ide).

It is not meant to be used as an actual tool for PureScript development yet...

## Prerequisits
You need a working installation of nodejs. Then do:

`npm install -g pulp purescript`

## Installation

1. Clone the repo:

    `git clone https://github.com/kRITZCREEK/psc-gui && cd psc-gui`

2. Install dependencies:

    `npm i && pulp dep i`

3. Build the project

    `pulp browserify --main Browser --to app.js`

4. Start psc-ide-server in a PureScript project. Luckily psc-gui is one of
   these, so open another terminal, navigate to psc-gui and execute:

   `psc-ide-server --debug`

    (*The --debug flag makes the server print out all requests*)

5. Run the application:

   `node_modules/.bin/electron .`
