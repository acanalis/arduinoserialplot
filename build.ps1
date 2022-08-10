elm make --output static/main.js src/elm/Main.elm
go build -o server.exe "./src/go/server.go" 
./server.exe
