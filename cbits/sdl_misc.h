#ifndef SDL_MISC_H
#define SDL_MISC_H

#define SDL_MAIN_HANDLED
#include <SDL2/SDL.h>

Uint32 get_mask(const Uint32 b);

SDL_version* compiled_version();

#endif // SDL_MISC_H
