#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <u.h>
#include <lib9.h>
#include <draw.h>
#include <cursor.h>
#include <event.h>

#include "9.h"

extern int screenw, screenh;
extern bool, lbtn_p, rbtn_p;

typedef enum {
  MOUSE_BUTTON_LEFT    = 0,
  MOUSE_BUTTON_RIGHT   = 1,
  MOUSE_BUTTON_MIDDLE  = 2,
} rlMouseButtons;

typedef struct rlVector2 {
    float x;
    float y;
} rlVector2;

#define Vector2 rlVector2

typedef struct rlVector3 {
    float x;
    float y;
    float z;
} rlVector3;

typedef struct rlVector4 {
    float x;
    float y;
    float z;
    float w;
} rlVector4;

typedef struct rlColor {
    unsigned char r;
    unsigned char g;
    unsigned char b;
    unsigned char a;
} Color;

typedef struct rlRectangle {
    float x;
    float y;
    float width;
    float height;
} rlRectangle;

typedef struct rlImage {
    void *data;
    int width;
    int height;
    int mipmaps;
    int format;
} rlImage;

typedef struct rlTexture {
    unsigned int id;
    int width;
    int height;
    int mipmaps;
    int format;
} rlTexture;

typedef struct rlGlyphInfo {
    int value;
    int offsetX;
    int offsetY;
    int advanceX;
    rlImage image;
} rlGlyphInfo;

typedef struct rlFont {
    int baseSize;
    int glyphCount;
    int glyphPadding;
    rlTexture texture;
    rlRectangle *recs;
    rlGlyphInfo *glyphs;
} rlFont;

struct window_conf_t {
  uint32_t bgcolor;
  uint32_t mirror_color;
  uint32_t prism_outline_color;
  uint32_t lens_outline_color;
  uint32_t lens_center_color;
  uint32_t lens_focal_pt_color;
  uint32_t source_color;

  int state;
};

extern struct window_conf_t winconf;

extern char* argv0;
static Image *bg;
static Event e;

static int xr, yr;

void
redraw(Image *screen)
{
  xr = screen->r.min.x, yr = screen->r.min.y;

  screenw = screen->r.max.x - xr;
  screenh = screen->r.max.y - yr;
}

#define color2color(color) \
  (((color>>0)&0xff)<<24)| \
  (((color>>8)&0xff)<<16)| \
  (((color>>16)&0xff)<<8)| \
  (((color>>24)&0xff)<<0)

void
new_bgcolor(uint32_t c)
{
  winconf.bgcolor = c;
  bg = allocimage(display, Rect(0, 0, 1, 1), RGB24, 1, c);
}

void
ClearBackground(uint32_t color)
{
  if (color == color2color(winconf.bgcolor))
    draw(screen, screen->r, bg, nil, ZP);
  else {
    new_bgcolor(color);
    ClearBackground(color);
  }
}

void
eresized(int new)
{
  xr = screen->r.min.x, yr = screen->r.min.y;
  if (new && getwindow(display, Refnone) < 0)
    warnx("%s: can't reattach window", __FILE__);
  redraw(screen);
}

void
DrawTextEx(rlFont _, char *s, Vector2 pos, float size, float spacing, uint32_t color)
{
  Image *tmp_img = allocimage(display, Rect(0, 0, 1, 1), RGB24, 1, color2color(color));
  Point pt = string(screen, Pt((int)xr + pos.x, (int)yr + pos.y), tmp_img, ZP, font, s);
}

void
DrawLineEx(rlVector2 p1, rlVector2 p2, float thick, uint32_t color)
{
  Image *tmp_img = allocimage(display, Rect(0, 0, 1, 1), RGB24, 1, color2color(color));
  line(screen, Pt(p1.x+xr, p1.y+yr), Pt(p2.x+xr, p2.y+yr), 0, 0, thick, tmp_img, ZP);
}

void
DrawTriangleLines(Vector2 v1, Vector2 v2, Vector2 v3, uint32_t color)
{
  DrawLineEx(v1, v2, 1, color);
  DrawLineEx(v2, v3, 1, color);
  DrawLineEx(v3, v1, 1, color);
}

void
DrawRectanglePro(rlRectangle rec, rlVector2 origin, float rotation, uint32_t color)
{
  Image *tmp_img = allocimage(display, Rect(0, 0, 1, 1), RGB24, 1, color2color(color));
  draw(screen, Rect(xr+rec.x-origin.x, yr+rec.y-origin.y, xr+rec.x + rec.width-origin.x, yr+rec.y + rec.height-origin.y), tmp_img, nil, ZP);
}

void
DrawRectangle(int x, int y, int w, int h, uint32_t c)
{
  rlRectangle rec = {x, y, w, h};
  DrawRectanglePro(rec, (rlVector2){0,0}, 0, c);
}

void
InitWindow(int w, int h, char *title)
{
  if (initdraw(nil, nil, argv0) < 0)
    perror(argv0);
  uint32_t color = *(uint32_t*)&winconf.bgcolor;
  einit(Emouse|Ekeyboard);

  //extern Image *screen;
  bg = allocimage(display, Rect(0, 0, 1, 1), RGB24, 1, color2color(color));

  redraw(screen);
}

extern rlVector2 mpos;
extern cpressed;
void
BeginDrawing(void)
{
  redraw(screen);

  Event e;
  Mouse m;
  int key = event(&e);
  m = e.mouse;
  mpos.x = m.xy.x - xr;
  mpos.y = m.xy.y - yr;
  cpressed = 0;
  switch (key) {
    case Emouse:
      rbtn_p = (m.buttons & 4);
      lbtn_p = (m.buttons & 1);
      break;
    case Ekeyboard:
      cpressed = e.kbdc;
      warnx("%d: %c", cpressed, cpressed);
      break;
  }
}

void
EndDrawing(void)
{
  flushimage(display, Refnone);
}

