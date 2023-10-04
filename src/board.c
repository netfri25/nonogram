#include "board.h"

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

void board_fill_between(
    Board* const self,

    int const x1,
    int const y1,
    int const x2,
    int const y2,

    enum Cell const cell,
    enum Cell const apply_to
) {
    // https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm#All_cases
    int const dx = abs(x2 - x1);
    int const sx = x1 < x2 ? 1 : -1;
    int const dy = -abs(y2 - y1);
    int const sy = y1 < y2 ? 1 : -1;

    int error = dx + dy;
    int x = x1;
    int y = y1;

    for (;;) {
        if (board_get(self, x, y) == apply_to)
            board_set(self, x, y, cell);

        if (x == x2 && y == y2) break;

        int const e2 = 2 * error;
        if (e2 >= dy) {
            if (x == x2) break;
            error += dy;
            x += sx;
        }

        if (e2 <= dx) {
            if (y == y2) break;
            error += dx;
            y += sy;
        }
    }
}
