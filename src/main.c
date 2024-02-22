#include "optyka.h"
#include "raylib.h"
#include "raymath.h"
#include "tinyscheme/scheme.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include <stdbool.h>

#ifndef PLAN9
#include <unistd.h>
char *argv0;
#endif

#define DEBUG 0
#undef DEBUG

extern bool scheme_is_initialized;
extern scheme scm;
extern hookable_event_t keypress, click, unclick, frame, clocke, loge, resize, filesdropped;

#define MAX_INPUT_BUFFER_SIZE 4096
/* static void (*input_func)(void) = NULL; */
/* static char input_buffer[MAX_INPUT_BUFFER_SIZE] = {0}; */

Font fontset[MAX_FONT_SIZE] = {0};

/* to jest nieco niespójne ze scheme podejście, ale co klatkę wykonywane jest
   ClearBackground(), i robienie tego w scheme było by po prostu zbyt powolne */
struct window_conf_t winconf;

void init_winconf(void)
{
  winconf.bgcolor             = (Color){ 0x2b, 0x33, 0x39, 0xff };
  winconf.mirror_color        = (Color){ 0x7f, 0xbb, 0xb3, 0xff };
  winconf.prism_outline_color = (Color){ 0xff, 0x00, 0xff, 0xff };
  winconf.lens_outline_color  = (Color){ 0xdb, 0xbc, 0x7f, 0xff };
  winconf.lens_center_color   = (Color){ 0x9d, 0xa9, 0xa0, 0xaa };
  winconf.lens_focal_pt_color = (Color){ 0xe6, 0x98, 0x75, 0xff };
  winconf.source_color        = (Color){ 0xff, 0x00, 0xff, 0xff };
}

#define MIRROR_THICKNESS 1

Bounceables bounceables = {0};
Sources sources = {0};

bool dont_tracelog = false;

Font get_font_with_size(int size)
{
  extern unsigned int proggy_otf_len;
  extern unsigned char proggy_otf[];

  if (fontset[size].baseSize == 0) {
    fontset[size] = LoadFontFromMemory(".otf", proggy_otf, proggy_otf_len, size, NULL, 1024);
    SetTextureFilter(fontset[size].texture, TEXTURE_FILTER_POINT);
    TraceLog(LOG_INFO, "loaded default font with size %d", size);
  }

  return fontset[size];
}

Color dim_color(Color c, int alpha)
{
  return (Color){c.r, c.g, c.b, alpha};
}

void *rl_get_window_handle(void)
{
  return GetWindowHandle();
}

float normalize_angle(float f)
{
  while (f >= 360) f -= 360.f;
  while (f < 0) f += 360.f;

  return f;
}

// bo CheckCollisionPointPoly z raylib 4.5 nie działa poprawnie
// naprawione w raylib 5, C-c C-v tutaj
bool collision_point_poly(Vector2 point, Vector2 *points, int pointCount)
{
  bool inside = false;

  if (pointCount > 2)
    for (int i = 0, j = pointCount - 1; i < pointCount; j = i++)
      if ((points[i].y > point.y) != (points[j].y > point.y) &&
          (point.x < (points[j].x - points[i].x) * (point.y - points[i].y) / (points[j].y - points[i].y) + points[i].x))
        inside = !inside;

  return inside;
}

// via ./notatki.ora
Vector2 create_target(Vector2 a, float angle)
{
  const int H = GetScreenHeight();

  if ((angle > 180 && angle <= 360) || angle == 0)
    return (Vector2){(ctg((180-angle)*(PI/180.f))*a.y+a.x), 0};
  else
    return (Vector2){a.x + ((H - a.y) / ctg(normalize_angle(90-angle)*DEG2RAD)), H};
}

static void draw_mirror(bounceable_t *b)
{
  Vector2 p1 = b->data.mirror->p1,
          p2 = b->data.mirror->p2;
  DrawLineEx(p1, p2, MIRROR_THICKNESS, winconf.mirror_color);
}

static void draw_all_bounceables(void)
{
  int i;
  for (i = 0; i < bounceables.n; ++i) {
    if (!bounceables.v[i].removed) {
      switch (bounceables.v[i].t) {
      case B_MIRROR:
        draw_mirror(&bounceables.v[i]);
        break;
      case B_LENS:
        draw_lens(&bounceables.v[i]);
        break;
      case B_PRISM:
        draw_prism(&bounceables.v[i]);
        break;
      case B_CUSTOM:
        draw_custom(&bounceables.v[i]);
        break;
      default:
        panic("unreachable");
      }
    }
  }
}

// jeśli miałoby coś się ścinać, zwiększże **TROCHĘ** tą liczbę żeby zwiększyć
// wydajność kosztem dokładności
#define CAST_LIGHT_STEP_SIZE 1
bool cast_light(Vector2 target, Vector2 source, Vector2 *ret, bounceable_t *hit_bounceable)
{
  int max_iter = 4096, i;
  while (max_iter) {
    source = Vector2MoveTowards(source, target, CAST_LIGHT_STEP_SIZE);

    for (i = 0; i < bounceables.n; ++i) {
      if (!bounceables.v[i].removed) {
        switch (bounceables.v[i].t) {
        case B_MIRROR:
          if (CheckCollisionPointLine(source,
                                      bounceables.v[i].data.mirror->p1,
                                      bounceables.v[i].data.mirror->p2,
                                      CAST_LIGHT_STEP_SIZE))
            goto hit;
          break;
        case B_LENS:
          if (collision_point_lens(source, bounceables.v[i].data.lens))
            goto hit;
          break;
        case B_PRISM:
          if (CheckCollisionPointTriangle(source,
                                          bounceables.v[i].data.prism->p1,
                                          bounceables.v[i].data.prism->p2,
                                          bounceables.v[i].data.prism->p3))
            goto hit;
          break;
        case B_CUSTOM: {
          customb_data_t *cd = bounceables.v[i].data.custom;
          /* for (int i = 0; i < cd->poly_pts; ++i) { */
          /*   DrawLineV(cd->poly[i], cd->poly[(i+1) % cd->poly_pts], WHITE); */
          /* } */
          if (cd->poly_pts == 2) {
            if (CheckCollisionPointLine(source, cd->poly[0], cd->poly[1], CAST_LIGHT_STEP_SIZE))
              goto hit;
          } else if (collision_point_poly(source, cd->poly, cd->poly_pts)) {
            goto hit;
          }
        } break;
        default:
          panic("unreachable");
        }
      }
    }

    max_iter--;
  }

  ret->x = source.x, ret->y = source.y, hit_bounceable = NULL;
  return false;

 hit:
  *hit_bounceable = bounceables.v[i];
  ret->x = source.x, ret->y = source.y;
  return true;
}

Vector2 create_target_by_hit(bounceable_t *b, Vector2 cur, Vector2 next, struct _teleport *tp, source_t *cur_src)
{
  switch (b->t) {
  case B_MIRROR: {
    float hit_angle = normalize_angle(Vector2Angle(b->data.mirror->p1, b->data.mirror->p2) * 180 / PI);
    float rel_angle = normalize_angle(hit_angle - normalize_angle(Vector2Angle(cur, next) * 180 / PI));
    float cur_angle = normalize_angle(hit_angle + rel_angle);
    return create_target(next, cur_angle);
  } break;
  case B_LENS: {
    return lens_create_target(b->data.lens, cur, next, tp, cur_src);
    /* lens_data_t *ld = b->data.lens; */
  } break;
  case B_PRISM: {
    return prism_create_target(b, cur, next, tp, cur_src);
  } break;
  case B_CUSTOM: {
    float ang;
    custom_get_light_remap(b->data.custom, next, normalize_angle(Vector2Angle(cur, next) * RAD2DEG), tp, &ang);
    Vector2 targ = create_target(tp->luzik, normalize_angle(ang));
    tp->luzik = Vector2MoveTowards(tp->luzik, targ, 2);

    return targ;
  } break;
  default:
    panic("unreachable");
    return vec(0, 0);
  }
}

// https://www.physicsclassroom.com/class/refln/Lesson-1/The-Law-of-Reflection
// nie zesraj sie krzysztof z przeszlosci oki?
#define max_draw_lines 100
#define draw_single_light(source, start, s_targ) _draw_single_light(source, start, s_targ, max_draw_lines)
void _draw_single_light(source_t *source, Vector2 start, Vector2 s_target, int max_depth)
{
  int i;
  Vector2 next, cur = {
    .x = start.x,
    .y = start.y
  }, cur_target = s_target;
  bounceable_t hit_bounceable = {0};
  bool bounced = true;
  struct _teleport tp = {0};

  for (i = 0; i < max_depth && bounced; ++i) {
    tp.serio = 0;
    bounced = cast_light(cur_target, cur, &next, &hit_bounceable);
    DrawLineEx(cur, next, source->thickness, source->color);

    if (bounced) {
      cur_target = create_target_by_hit(&hit_bounceable, cur, next, &tp, source);
      //cur_target = create_target(next, cur_angle);
      cur = Vector2MoveTowards(next, cur_target, 1);
    }

    // ja nawet nie próbuję udawać że ten kodzik robi sens
    // będę się starał pisząc scheme a nie to
    if (tp.do_trzeciej_warstwy_piekla)
      return;
    if (tp.serio) {
      cur = tp.luzik;
    }
  }
}

void draw_light(source_t *src)
{
  int i, dist = floor(src->size / (1.f+(float)src->n_beam));
  for (i = 1; i <= src->n_beam; ++i) {
    /* int sz = ((float)src->size/2)/sqrt(2); */
    int sz = (((float)src->size/2)-(i*dist))/sqrt(2);
    Vector2 rot = Vector2Rotate(vec(sz, sz), (-45 - 90 + src->angle) * PI/180);
    Vector2 pt = vec(src->pt.x + rot.x, src->pt.y + rot.y);
    // witam nazywam sie krzysztof, a te wzory wyciagnalem prosto z dupy

    draw_single_light(src, pt, vec(src->target.x - (src->pt.x - pt.x),
                                   src->target.y - (src->pt.y - pt.y)));
  }
}

char *strBtype(bounceable_type_t t)
{
  switch (t) {
  case B_PRISM: return "B_PRISM";
  case B_MIRROR: return "B_MIRROR";
  case B_LENS: return "B_LENS";
  case B_CUSTOM: return "B_CUSTOM";
  }

  return "INVALID";
}


void add_bounceable(bounceable_type_t t, void *data)
{
  bounceable_t *b = malloc(sizeof(bounceable_t));
  b->t = t;
  b->data.p = data;
  b->removed = 0;

  dyn_add_ptr_sized((&bounceables), (*b), sizeof(bounceable_t));
}

void add_mirror(Vector2 p1, Vector2 p2)
{
  mirror_data_t *md = malloc(sizeof(mirror_data_t));
  md->p1 = (Vector2){p1.x,p1.y};
  md->p2 = (Vector2){p2.x,p2.y};

  add_bounceable(B_MIRROR, md);
}

static void silent_tracelog_callback(__attribute__((unused))int a,
                                     __attribute__((unused))const char *b,
                                     __attribute__((unused))va_list c)
{
}

static char *logtype2string(TraceLogLevel l)
{
  switch (l) {
  case LOG_TRACE: return "TRACE";
  case LOG_DEBUG: return "DEBUG";
  case LOG_INFO: return "INFO";
  case LOG_WARNING: return "WARNING";
  case LOG_ERROR: return "ERROR";
  case LOG_FATAL: return "FATAL";
  default: return "OTHER";
  }

  /* unreachable */
}

static pointer logtype2sym(TraceLogLevel l)
{
  switch (l) {
  case LOG_TRACE:   return mk_symbol(&scm, "trace");
  case LOG_DEBUG:   return mk_symbol(&scm, "debug");
  case LOG_INFO:    return mk_symbol(&scm, "info");
  case LOG_WARNING: return mk_symbol(&scm, "warning");
  case LOG_ERROR:   return mk_symbol(&scm, "error");
  case LOG_FATAL:   return mk_symbol(&scm, "fatal");
  default:          return mk_symbol(&scm, "other");
  }

}

// $ cd /path/to/raylib-src
// $ find . -type f -name '*.c' -exec grep -io 'TraceLog(.*)' '{}' ';' | cut -f2 -d'"' | grep -oP '[A-Z]+(?=:)' | sort | uniq | sed 's/^/"/ ; s/$/",/' | xclip -sel c

static const char *rl_tracelog_tokens[] = {
  "ANDROID", "AUDIO", "DISPLAY", "DOWN",
  "FBO", "FILEIO", "FONT", "GESTURE",
  "GLFW", "IMAGE", "INPUT", "KEY",
  "MATERIAL", "MESH", "MODEL", "MOTION",
  "POSITION", "RLGL", "RPI", "SOUND",
  "STREAM", "SYSTEM", "TEXTURE", "TIMER",
  "UP", "VAO", "VBO", "WAVE", "WINDOW",
};

static bool is_raylib_message(char *s)
{
#ifdef PROD
  int i;
  for (i = 0; i < sizeof(rl_tracelog_tokens)/sizeof(*rl_tracelog_tokens); ++i)
    if (strstr(s, rl_tracelog_tokens[i]) != NULL)
      return true;
#endif

  return false;
}

#define TRACELOG_BUFSIZE 8196
static char tracelog_buf[TRACELOG_BUFSIZE];
static void tracelog_cb(int type, const char *fmt, va_list vl)
{
  memset(tracelog_buf, 0, TRACELOG_BUFSIZE); // niepotrzebne lol
  vsnprintf(tracelog_buf, TRACELOG_BUFSIZE, fmt, vl);

  fprintf(stderr, "%s: %s\n", logtype2string(type), tracelog_buf);
  fflush(stderr);

  if (scheme_is_initialized) {
    if (!is_raylib_message(tracelog_buf))
      do_hooks(&loge,
               cons(&scm,
                    logtype2sym(type),
                    cons(&scm,
                         mk_string(&scm, tracelog_buf),
                         scm.NIL)));
  }
}

#ifdef _WIN32
int WinMain(void *hInstance, __attribute__((unused))void *prevInstance,
            __attribute__((unused))void *a, __attribute__((unused))int b)
#else // _WIN32
int main(int argc, char **argv)
#endif // _WIN32
{
  extern scheme scm;
  extern char *argv0;
  argv0 = strdup(*argv);
  int i, c, k, charsize, opt;
  time_t time_prev, time_cur;
  struct mouse_information_t mi = {
    .first_click = false,
    .pos = {0,0},
    .pressed_moving = false,
    .left = false,
    .right = false,
  };
  init_winconf();

  SetTraceLogCallback(tracelog_cb);
#if !defined(_WIN32) && !defined(PLAN9)
  while ((opt = getopt(argc, argv, "e:F:")) != -1) {
    switch (opt) {
    case 'e':
      initialize_scheme();
      scheme_load_string(&scm, optarg);
      exit(0);
      break;
    case 'F':
      dont_tracelog = true;
      SetTraceLogCallback(silent_tracelog_callback);
      initialize_scheme();
      FILE *f = fopen(optarg, "r");
      assert(f != NULL);
      scheme_load_file(&scm, f);
      exit(0);
      break;
    }
  }
#endif

  // TODO: idk czy resizable + bardzo duzo nowego scheme
  // czy Camera2D + [wasd] + malo nowego kodu
  /* SetConfigFlags(FLAG_WINDOW_RESIZABLE); */

  SetTargetFPS(60);

#ifdef ANTIALIAS
  SetConfigFlags(FLAG_MSAA_4X_HINT);
#endif

  InitWindow(800, 600, "giga optyka");
  initialize_scheme();
  SetExitKey(-1);
  load_rc();

#ifdef _WIN32
  extern void *win_hinstance;
  win_hinstance = hInstance;
  w32_load_icon();
#endif

  /* add_prism(vec(200, 200), 100, 1.31); */
  /* add_lens(vec(200, 200), vec(200, 300), 20.f, 200.f); */

  time_prev = time_cur = time(NULL);
  while (!WindowShouldClose()) {
    update_screen_size_variables();
    mi.pos = GetMousePosition();

    BeginDrawing();
    {
      ClearBackground(winconf.bgcolor);

      c = GetCharPressed();
      k = GetKeyPressed();
      if (c || k)
        do_hooks(&keypress, cons(&scm, mk_character(&scm, c),
              cons(&scm, mk_integer(&scm, k), scm.NIL)));

      if ((IsMouseButtonDown(MOUSE_BUTTON_LEFT) ||
            IsMouseButtonDown(MOUSE_BUTTON_RIGHT))
          && mi.pressed_moving == false) {
        mi.pressed_moving = true;
        mi.first_click = true;

        // bardzo nie podoba mi się fakt, że nie moge tego sprawdzić jakimiś
        // flagami typu IsMouseButtonDown(L | R);
        // (sprawdzałem implementację - nie mogę)
        if (IsMouseButtonDown(MOUSE_LEFT_BUTTON))
          mi.left = true;

        if (IsMouseButtonDown(MOUSE_RIGHT_BUTTON))
          mi.right = true;
      }

      if (!IsMouseButtonDown(MOUSE_BUTTON_LEFT) &&
          !IsMouseButtonDown(MOUSE_BUTTON_RIGHT)
          && mi.pressed_moving) {
        do_hooks(&unclick, scheme_click_info(&mi));

        mi.pressed_moving = mi.first_click = mi.left = mi.right = false;
      }

      if (winconf.state == state_running) {
        draw_all_bounceables();

        // TODO: to powinno być w osobnej funkcji. ale to kiedyś
        for (i = 0; i < sources.n; ++i) {
          draw_source(&sources.v[i]);
          draw_light(&sources.v[i]);
        }
      }

      if (mi.pressed_moving) {
        do_hooks(&click, scheme_click_info(&mi));
      }

    }

    do_hooks(&frame, scm.NIL);
    if ((time_cur = time(NULL)) != time_prev) {
      time_prev = time_cur;
      do_hooks(&clocke, cons(&scm, mk_integer(&scm, time_cur),
                             scm.NIL));
    }

    if (IsFileDropped()) {
      FilePathList fpl = LoadDroppedFiles();
      do_hooks(&filesdropped, fpl2list(fpl));
      UnloadDroppedFiles(fpl);
    }

    EndDrawing();

    mi.first_click = false;
  }

  if (sources.v)
    free(sources.v);
  if (bounceables.v)
    free(bounceables.v);
  scheme_deinit(&scm);

  return 0;
}
