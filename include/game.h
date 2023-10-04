#pragma once

#include "board.h"

typedef struct {
    int* items;
    size_t count;
    size_t capacity;
} IntVec;

typedef struct {
    IntVec* items;
    size_t count;
    size_t capacity;
} IntVecVec;

typedef struct {
    Board board;
    Board solution;
    IntVecVec cols_groups;
    IntVecVec rows_groups;
} Game;

// allocate a new game with the given dimensions
Game game_alloc(size_t const w, size_t const h);

// deallocate the game
void game_free(Game const self);

// generate a new game
void game_generate_new(Game* const self);

// set the cell at a given position
// fails when the position is out of bounds
bool game_set(Game* const self, size_t const x, size_t const y, enum Cell const cell);

// get the cell at a given position in the game board
// WARNING: will return CELL_EMPTY when the position is out of bounds
enum Cell game_get_board(Game const* const self, size_t const x, size_t const y);

// get the cell at a given position in the solution board
// WARNING: will return CELL_EMPTY when the position is out of bounds
enum Cell game_get_solution(Game const* const self, size_t const x, size_t const y);
