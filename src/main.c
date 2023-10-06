// TODO(2): create a solver as a separate program (maybe write it in Haskell, or even Prolog)

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <inttypes.h>

#include <raylib.h>
#include <math.h>
#include <time.h>

#include "game.h"

typedef struct {
    int32_t x;
    int32_t y;
} IVec2;

typedef struct {
    size_t size;
    Game game;
    IVec2 last_changed;
    MouseButton last_button;
    enum Cell to_fill;
    enum Cell fill_apply_to;

    Rectangle board_rect;

    size_t longest_row;
    size_t longest_col;

    float font_line_height;
    float font_line_width;
    float font_height;
    float font_width;

    Board* to_draw;
} App;

// utility functions
Rectangle get_screen_rect(void);
IVec2 get_mouse_indices(Rectangle const board_rect, Vector2 const mouse_position, int32_t const w, int32_t const h);

// drawing functions
void draw_board(Board const* const board, Rectangle const bounds, IVec2 const mouse_indices);
void draw_cell(float const x, float const y, float const w, float const h, enum Cell const cell, bool const mouse_inside);
void draw_grid(Rectangle const bounds, float const w, float const h);
void draw_x(float const x, float const y, float const w, float const h);
void draw_text(App const* const app);

// calculations for the app
void calc_longest_row_and_col(App* const app);
void calc_board_rect(App* const app);
void calc_font_sizes(App* const app);

// main functions
void handle_input(App* const app);
void draw_app(App const* const app);

// global definitions
#define GRID_THICK 2.f
#define GRID_COLOR GRAY
#define BUF_CAP 16

#define FPS 45.f
#define TIME_BETWEEN_FRAMES (CLOCKS_PER_SEC / FPS)

int main(void) {
    SetTraceLogLevel(LOG_WARNING);
    InitWindow(850, 850, "Window");

    App app = {0};

    app.size = 7;
    app.game = game_alloc(app.size, app.size);
    app.last_changed = (IVec2) { -1, -1 };
    app.last_button = MOUSE_BUTTON_LEFT;
    app.to_fill = CELL_REMOVE;
    app.to_draw = &app.game.board;

    game_generate_new(&app.game);
    calc_longest_row_and_col(&app);
    calc_font_sizes(&app);
    clock_t last_update = 0;
    while (!WindowShouldClose()) {
        clock_t now = clock();
        if (now - last_update < TIME_BETWEEN_FRAMES)
            continue;
        last_update = now;

        calc_font_sizes(&app);
        calc_board_rect(&app);
        draw_app(&app);
        handle_input(&app);
        DrawFPS(0, 0);
    }

    CloseWindow();
    game_free(app.game);
}

void handle_input(App* const app) {
    Vector2 const mouse_position = GetMousePosition();
    IVec2 const mouse_i = get_mouse_indices(app->board_rect, mouse_position, app->game.board.w, app->game.board.h);

    if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        enum Cell const here = board_get(&app->game.board, mouse_i.x, mouse_i.y);
        app->last_button = MOUSE_BUTTON_LEFT;
        app->to_fill = (here != CELL_FILL) * CELL_FILL;
        app->fill_apply_to = here;
    }

    if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT)) {
        enum Cell const here = board_get(&app->game.board, mouse_i.x, mouse_i.y);
        app->last_button = MOUSE_BUTTON_RIGHT;
        app->to_fill = (here != CELL_REMOVE) * CELL_REMOVE;
        app->fill_apply_to = here;
    }

    if (IsMouseButtonDown(app->last_button) && !board_is_out(&app->game.board, mouse_i.x, mouse_i.y)) {
        bool const no_last_changed = app->last_changed.x == -1 && app->last_changed.y == -1;
        IVec2 const snd = no_last_changed ? mouse_i : app->last_changed;
        board_fill_between(
            &app->game.board,
            snd.x,
            snd.y,
            mouse_i.x,
            mouse_i.y,
            app->to_fill,
            app->fill_apply_to
        );
        app->last_changed = mouse_i;
    }

    if (IsMouseButtonReleased(app->last_button)) {
        app->last_changed = (IVec2) { -1, -1 };
    }

    if (IsKeyPressed(KEY_S)) {
        if (app->to_draw == &app->game.board) {
            app->to_draw = &app->game.solution;
        } else {
            app->to_draw = &app->game.board;
        }
    }
}

void draw_app(App const* const app) {
    BeginDrawing();

    Vector2 const mouse_position = GetMousePosition();
    IVec2 const mouse_i = get_mouse_indices(app->board_rect, mouse_position, app->game.board.w, app->game.board.h);

    ClearBackground(BLACK);
    draw_text(app);
    draw_board(app->to_draw, app->board_rect, mouse_i);
    draw_grid(app->board_rect, app->game.board.w, app->game.board.h);
    EndDrawing();
}

void calc_board_rect(App* const app) {
    Rectangle const bounds = get_screen_rect();

    float const size = bounds.width < bounds.height ? bounds.width : bounds.height;
    float const x = bounds.x + (bounds.width  - size) / 2.;
    float const y = bounds.y + (bounds.height - size) / 2.;

    float const x_text_pixels = app->longest_row * app->font_line_width;
    float const y_text_pixels = app->longest_col * app->font_line_height;

    app->board_rect = (Rectangle) {
        .x = x + x_text_pixels,
        .y = y + y_text_pixels,
        .width  = size - x_text_pixels * 2.,
        .height = size - y_text_pixels * 2.,
    };
}

void draw_board(Board const* const board, Rectangle const bounds, IVec2 const mouse_indices) {
    float const cell_w = bounds.width / board->w;
    float const cell_h = bounds.height / board->h;

    for (size_t ix = 0; ix < board->w; ix++) {
        for (size_t iy = 0; iy < board->h; iy++) {
            float const x = bounds.x + ix * cell_w;
            float const y = bounds.y + iy * cell_h;
            bool const mouse_inside = mouse_indices.x == (int32_t) ix && mouse_indices.y == (int32_t) iy;
            enum Cell const cell = board_get(board, ix, iy);
            draw_cell(x, y, cell_w, cell_h, cell, mouse_inside);
        }
    }
}

void draw_cell(
    float const x,
    float const y,
    float const w,
    float const h,
    enum Cell const cell,

    bool const mouse_inside
) {
    Color background;
    if (cell == CELL_FILL) {
        background = BLACK;
    } else {
        background = RAYWHITE;
    }

    if (mouse_inside) {
        background.r = ((int) background.r + 128) / 2;
        background.g = ((int) background.g + 128) / 2;
        background.b = ((int) background.b + 128) / 2;
    }

    DrawRectangle(x, y, roundf(w), roundf(h), background);
    if (cell == CELL_REMOVE) {
        draw_x(x, y, w, h);
    }
}

void draw_grid(Rectangle const bounds, float const w, float const h) {
    float const cell_w = bounds.width / w;
    for (size_t i = 0; i <= w; i++) {
        float const x = bounds.x + i * cell_w;
        Vector2 const start = { x, bounds.y };
        Vector2 const end   = { x, bounds.y + bounds.height };
        DrawLineEx(start, end, GRID_THICK, GRID_COLOR);
    }

    float const cell_h = bounds.height / h;
    for (size_t i = 0; i <= h; i++) {
        float const y = bounds.y + i * cell_h;
        Vector2 const start = { bounds.x, y };
        Vector2 const end   = { bounds.x + bounds.width, y };
        DrawLineEx(start, end, GRID_THICK, GRID_COLOR);
    }
}

void draw_x(float const x, float const y, float const w, float const h) {
    float const thick = (w + h) * 0.1 / 2.;
    float const pad = thick;
    Vector2 const tl = (Vector2) { x + pad,     y + pad };
    Vector2 const tr = (Vector2) { x + w - pad, y + pad };
    Vector2 const bl = (Vector2) { x + pad,     y + h - pad };
    Vector2 const br = (Vector2) { x + w - pad, y + h - pad };
    DrawLineEx(tl, br, thick, RED);
    DrawLineEx(bl, tr, thick, RED);
}

IVec2 get_mouse_indices(Rectangle const board_rect, Vector2 const mouse_position, int32_t const w, int32_t const h) {
    if (CheckCollisionPointRec(mouse_position, board_rect)) {
        return (IVec2) {
            .x = (mouse_position.x - board_rect.x) * (float) w / board_rect.width,
            .y = (mouse_position.y - board_rect.y) * (float) h / board_rect.height,
        };
    } else {
        return (IVec2) { -1, -1 };
    }

}

Rectangle get_screen_rect(void) {
    return (Rectangle) {
        .x = 0,
        .y = 0,
        .width = GetScreenWidth(),
        .height = GetScreenHeight(),
    };
}

void calc_longest_row_and_col(App* const app) {
    size_t longest_row = 0;
    for (size_t i = 0; i < app->game.rows_groups.count; i++) {
        size_t const length = app->game.rows_groups.items[i].count;
        if (length > longest_row)
            longest_row = length;
    }

    size_t longest_col = 0;
    for (size_t i = 0; i < app->game.cols_groups.count; i++) {
        size_t const length = app->game.cols_groups.items[i].count;
        if (length > longest_col)
            longest_col = length;
    }

    app->longest_row = longest_row;
    app->longest_col = longest_col;
}

void calc_font_sizes(App* const app) {
    Rectangle const bounds = get_screen_rect();
    float const size = bounds.width < bounds.height ? bounds.width : bounds.height;
    app->font_line_height = size / (app->game.board.h + (float) app->longest_col * 2);
    app->font_line_width  = size / (app->game.board.w + (float) app->longest_row * 2);
    app->font_height = app->font_line_height * 0.6;
    app->font_width  = app->font_height / 2.;
}

void draw_text(App const* const app) {
    // draw the rows text on the left
    for (size_t i = 0; i < app->game.rows_groups.count; i++) {
        float const y = app->board_rect.y + i * app->font_line_height + (app->font_line_height - app->font_height) / 2.;

        IntVec const* const list = app->game.rows_groups.items + i;
        for (size_t j = 0; j < list->count; j++) {
            char buf[BUF_CAP];
            float const x = (app->board_rect.x - app->longest_row * app->font_line_width) + (app->longest_row - j - 1) * app->font_line_width;
            snprintf(buf, BUF_CAP, "%d", list->items[list->count - j - 1]);
            int const length = MeasureText(buf, app->font_height);
            float const x_off = (app->font_line_width - length) / 2.;
            DrawText(buf, x + x_off, y, app->font_height, WHITE);
        }
    }

    // draw the cols text on the top
    for (size_t i = 0; i < app->game.cols_groups.count; i++) {
        float const x = app->board_rect.x + i * app->font_line_width;

        IntVec const* const list = app->game.cols_groups.items + i;
        for (size_t j = 0; j < list->count; j++) {
            char buf[BUF_CAP];
            float const y
                = (app->board_rect.y - app->longest_col * app->font_line_height)
                + (app->longest_col - j - 1) * app->font_line_height
                + (app->font_line_height - app->font_height) / 2.;
            snprintf(buf, BUF_CAP, "%d", list->items[list->count - j - 1]);
            int const length = MeasureText(buf, app->font_height);
            float const x_off = (app->font_line_width - length) / 2.;
            DrawText(buf, x + x_off, y, app->font_height, WHITE);
        }
    }
}
