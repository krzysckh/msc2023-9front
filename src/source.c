#include "optyka.h"
#include "raylib.h"

#include <stdlib.h>

extern Sources sources;
extern struct window_conf_t winconf;

void add_source(source_t s)
{
  if (s.n_beam >= s.size) {
    TraceLog(LOG_WARNING, "n_beam cannot be higher or equal size");
    s.n_beam = s.size-1;
  }

  source_t src;
  src.color = (Color){s.color.r, s.color.g, s.color.b, s.color.a},
  src.pt = (Vector2){ s.pt.x, s.pt.y },
  src.angle = normalize_angle(s.angle),
  src.size = s.size,
  src.thickness = s.thickness,
  src.mouse_reactive = s.mouse_reactive,
  src.n_beam = s.n_beam;

  dyn_add(sources, src);
}

void draw_source(source_t *s)
{
  Vector2 cur_mouse;
  Rectangle rect = {
    .x = s->pt.x,
    .y = s->pt.y,
    .width = s->size,
    .height = s->size
  };

  cur_mouse = GetMousePosition();

  if (s->mouse_reactive)
    s->angle = normalize_angle(Vector2Angle((Vector2){rect.x, rect.y}, cur_mouse) * RAD2DEG);
  s->target = create_target((Vector2){rect.x, rect.y}, s->angle);

  DrawRectanglePro(rect, (Vector2){s->size / 2.f, s->size / 2.f}, s->angle, winconf.source_color);
  /* DrawCircleV(s->pt, 3, VIOLET); */
}
