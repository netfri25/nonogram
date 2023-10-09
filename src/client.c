#include "client.h"
#include <sys/socket.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#define CLIENT_ERR(msg) \
    do { if (self->log) fprintf(self->log, "ERROR: %s: %s\n", msg, strerror(errno)); self->connected = false; return false; } while (0)

#define CLIENT_LOG(...) \
    do { if (self->log) fprintf(self->log, "INFO: " __VA_ARGS__); } while (0)

Client client_alloc(FILE* const log) {
    return (Client) {
        .log = log,
        .socket_fd = 0,
        .server_addr = {0},
        .connected = false,
    };
}

void client_free(Client self) {
    if (self.socket_fd > 0) close(self.socket_fd);
}

bool client_connect(
    Client* const self,
    in_port_t const port,
    in_addr_t const addr
) {
    int res;

    res = self->socket_fd = socket(CLIENT_SOCKET_FAMILY, CLIENT_SOCKET_TYPE, 0);
    if (res < 0) CLIENT_ERR("client socket creation failed");
    CLIENT_LOG("client socket creation success\n");

    self->server_addr.sin_family = CLIENT_SOCKET_FAMILY;
    self->server_addr.sin_port = htons(port);
    self->server_addr.sin_addr.s_addr = htonl(addr);
    res = connect(self->socket_fd, (struct sockaddr*) &self->server_addr, sizeof self->server_addr);
    if (res < 0) CLIENT_ERR("client connect failed");
    CLIENT_LOG("client connect success\n");
    self->connected = true;
    return true;
}

bool client_send(
    Client* const self,
    void const* const data,
    size_t const len,
    size_t* const sent_count
) {
    int res;
    res = write(self->socket_fd, data, len);
    if (res <= 0) CLIENT_ERR("client write failed");
    if (sent_count) *sent_count = res;
    CLIENT_LOG("written %zu bytes to server\n", len);
    return true;
}

bool client_read(
    Client* const self,
    void* const data,
    size_t const cap,
    size_t* const read_count
) {
    int res;
    res = read(self->socket_fd, data, cap);
    if (res <= 0) CLIENT_ERR("client read failed");
    if (read_count) *read_count = res;
    CLIENT_LOG("read %zu bytes from server\n", *read_count);
    return true;
}
