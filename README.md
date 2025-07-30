# Nonogram
simple nonogram game writtern in C with [raylib](https://www.raylib.com/)

## Building
#### Dependencies
 - gcc
 - [raylib](https://www.raylib.com/) (already provided)
 - [swi-prolog](https://www.swi-prolog.org/) (optional. only for the server)

#### Running the application
first, build the [build tool](https://github.com/tsoding/nobuild)
```shell
gcc nob.c -o nob
```

then run it
```shell
./nob
```

after that, you can find your executable at `./build/main`, and simply execute:
```shell
./build/main
```

#### Running the server
simply just execute the `start-server.sh` file:
```shell
./start-server.sh
```

#### Keymaps
| key         | action                                                            |
| ----------- | ----------------------------------------------------------------- |
| c           | get a solution from the server (you need to run the server first) |
| s           | swap between the normal board and the solution                    |
| left click  | fill/clear a cell                                                 |
| right click | remove/clear a cell                                               |
