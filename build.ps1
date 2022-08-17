elm make src/elm/Main.elm --output static/main.js --debug
go build -o server.exe "./src/go/server.go" 
./server.exe -mock

