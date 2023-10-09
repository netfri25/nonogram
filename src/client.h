#pragma once

#include <stdio.h>
#include <stdbool.h>
#include <netinet/in.h>

#define CLIENT_SOCKET_FAMILY AF_INET
#define CLIENT_SOCKET_TYPE SOCK_STREAM
#define CLIENT_SOCKET_PROTOCOL 0

typedef struct {
    int socket_fd;
    struct sockaddr_in server_addr;
    FILE* log;
    bool connected;
} Client;

// construct a new client
Client client_alloc(FILE* const log);

// deconstruct and disconnect the client
void client_free(Client self);

// connect the client to the server
bool client_connect(
    Client* const self,
    in_port_t const port,
    in_addr_t const addr
);

// send data to the server
bool client_send(
    Client* const self,
    void const* const data,
    size_t const len,
    size_t* const sent_count
);

// read data from the server
bool client_read(
    Client* const self,
    void* const data,
    size_t const cap,
    size_t* const read_count
);
