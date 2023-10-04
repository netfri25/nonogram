#pragma once

#include <stdbool.h>
#include <stddef.h>

enum Cell {
    CELL_EMPTY = 0, // nothing has been set yet
    CELL_FILLED,    // fill the cell
    CELL_REMOVE,    // remove the cell, kinda like marking it with an X
};

typedef struct {
    enum Cell* cells;
    size_t w;
    size_t h;
} Board;

// allocate a new board with the given dimensions
Board board_alloc(size_t const w, size_t const h);

// deallocate the board, AKA "free"
void board_free(Board const self);

// set the cell at a given position
// fails when the position is out of bounds
bool board_set(Board* const self, size_t const x, size_t const y, enum Cell const cell);

// get the cell at a given position
// WARNING: will return CELL_EMPTY when the position is out of bounds
enum Cell board_get(Board const* const self, size_t const x, size_t const y);

// fill the board with random values
void board_fill_random(Board* const board);

// checks if the given position is out of bounds
bool board_is_out(Board const* const self, size_t const x, size_t const y);
