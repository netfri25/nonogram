#include "game.h"

#include "da.h"

#include <stdlib.h>
#include <string.h>

static void count_groups(Game* const self);

Game game_alloc(size_t const w, size_t const h) {
    Board const board = board_alloc(w, h);
    Board const solution = board_alloc(w, h);
    IntVecVec const cols_groups = {0};
    IntVecVec const rows_groups = {0};

    return (Game) {
        .board = board,
        .solution = solution,
        .cols_groups = cols_groups,
        .rows_groups = rows_groups,
    };
}

void game_free(Game const self) {
    if (self.rows_groups.items) {
        for (size_t i = 0; i < self.rows_groups.count; i++) {
            IntVec const item = self.rows_groups.items[i];
            if (item.items) free(item.items);
        }
        free(self.rows_groups.items);
    }

    if (self.cols_groups.items) {
        for (size_t i = 0; i < self.cols_groups.count; i++) {
            IntVec const item = self.cols_groups.items[i];
            if (item.items) free(item.items);
        }
        free(self.cols_groups.items);
    }

    board_free(self.board);
    board_free(self.solution);
}

void game_generate_new(Game* const self) {
    memset(self->board.cells, CELL_EMPTY, sizeof(*self->board.cells) * self->board.w * self->board.h);
    board_fill_random(&self->solution);
    self->rows_groups.count = 0;
    self->cols_groups.count = 0;
    count_groups(self);
}

bool game_set(Game* const self, size_t const x, size_t const y, enum Cell const cell) {
    return board_set(&self->board, x, y, cell);
}

enum Cell game_get_board(Game const* const self, size_t const x, size_t const y) {
    return board_get(&self->board, x, y);
}

enum Cell game_get_solution(Game const* const self, size_t const x, size_t const y) {
    return board_get(&self->solution, x, y);
}

static void count_groups(Game* const self) {
    size_t w = self->solution.w;
    size_t h = self->solution.h;

    // count groups in each row
    for (size_t y = 0; y < h; y++) {
        size_t x = 0;
        IntVec row_groups = {0};
        while (x < w) {
            // skip all non-filled cells
            for (; x < w && board_get(&self->solution, x, y) != CELL_FILLED; x++);

            // count cells
            size_t i;
            for (i = 0; x + i < w && board_get(&self->solution, x + i, y) == CELL_FILLED; i++);
            if (i == 0) break;
            da_append(&row_groups, i);
            x += i;
        }

        da_append(&self->rows_groups, row_groups);
    }

    // count groups in each col
    for (size_t x = 0; x < w; x++) {
        size_t y = 0;
        IntVec col_groups = {0};
        while (y < h) {
            // skip all non-filled cells
            for (; y < h && board_get(&self->solution, x, y) != CELL_FILLED; y++);

            // count cells
            size_t i;
            for (i = 0; y + i < h && board_get(&self->solution, x, y + i) == CELL_FILLED; i++);
            if (i == 0) break;
            da_append(&col_groups, i);
            y += i;
        }

        da_append(&self->cols_groups, col_groups);
    }
}
