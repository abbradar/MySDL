#include "sdl_misc.h"

Uint32 get_mask(const Uint32 b)
{
  return SDL_BUTTON(b);
}

SDL_version* compiled_version()
{
  static SDL_version version;
  SDL_VERSION(&version);
  return &version;
}
