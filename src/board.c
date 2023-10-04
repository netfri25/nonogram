#include "../include/board.h"

#include <stdlib.h>
#include <assert.h>

Board board_alloc(size_t const w, size_t const h) {
    enum Cell* const cells = (enum Cell*) calloc(w * h, sizeof(enum Cell));
    assert(cells && "no ram lol");
    return (Board) {
        .w = w,
        .h = h,
        .cells = cells,
    };
}

void board_free(Board const self) {
    if (self.cells) free(self.cells);
}

bool board_set(Board* const self, size_t const x, size_t const y, enum Cell const cell) {
    if (board_is_out(self, x, y)) return false;
    self->cells[y * self->w + x] = cell;
    return true;
}

enum Cell board_get(Board const* const self, size_t const x, size_t const y) {
    if (board_is_out(self, x, y)) return CELL_EMPTY;
    return self->cells[y * self->w + x];
}

void board_fill_random(Board* const self) {
    for (size_t i = 0; i < self->w * self->h; i++) {
        self->cells[i] = 1 + rand() % 2;
    }
}

bool board_is_out(Board const* const self, size_t const x, size_t const y) {
    return x >= self->w || y >= self->h;
}
