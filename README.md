# InfoMark-Frontend

InfoMark is a CI inspired course app. The goal is to achieve auto grading of exercises with unit tests.
This repo hosts the frontend of the application. It is written in [Elm](https://elm-lang.org/), a functional 
frontend language which compiles to JavaScript. The application is just a single page which uses fragments for
routing. So the server only needs to distribute the static HTML page and the REST Api which is used to 
interact with the server. The API is defined in this [repository](https://github.com/cgtuebingen/infomark-swagger)
using [Swagger](https://swagger.io/). 

The server is implemented in [Go](https://golang.org/) and is available [here](https://github.com/cgtuebingen/infomark-backend).

# Building

1. Install Elm with these [Instructions](https://guide.elm-lang.org/install.html)
2. Run `elm make src/Main.elm --output=elm.js` in the directory
3. Serve `index.html` with a server of your choice
