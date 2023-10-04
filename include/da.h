#pragma once

#include <stdlib.h>
#include <assert.h>

// Append an item to a dynamic array
#define da_append(da, item)                                                            \
    do {                                                                               \
        if ((da)->count >= (da)->capacity) {                                           \
            (da)->capacity = (da)->capacity == 0 ? 4 : (da)->capacity * 2;             \
            (da)->items = realloc((da)->items, (da)->capacity * sizeof(*(da)->items)); \
            assert((da)->items != NULL && "Buy more RAM lol");                         \
        }                                                                              \
        (da)->items[(da)->count++] = (item);                                           \
    } while (0)


#define da_free(da) free((da).items)
