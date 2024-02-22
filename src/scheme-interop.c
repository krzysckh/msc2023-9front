#include "optyka.h"
#include "raylib.h"
#include "raymath.h"
#include "tinyscheme/scheme-private.h"
#include "tinyscheme/scheme.h"

#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

#ifndef PLAN9
#include <unistd.h>
#endif

#ifdef PLAN9
#include "9.h"
#endif

/* funkcje zdefiniowane tutaj udokumentowane są w scm/cdocs.scm */

extern struct window_conf_t winconf;

pointer mk_port(scheme *sc, port *p);
pointer port_from_file(scheme *sc, FILE *f, int prop);
static void load_scheme_cfunctions(void);

scheme scm;
bool scheme_is_initialized = false;
static int last_screen_width = 0, last_screen_height = 0;

hookable_event_t keypress     = {0};
hookable_event_t click        = {0};
hookable_event_t unclick      = {0};
hookable_event_t resize       = {0};
hookable_event_t clocke       = {0}; // co sekundę
hookable_event_t loge         = {0}; // na każdy TraceLog()
hookable_event_t new          = {0}; // add_{mirror,lens}()
hookable_event_t update       = {0}; // set_{mirror,lens}()
hookable_event_t delete       = {0}; // {mirror,lens}...removed = 1
hookable_event_t filesdropped = {0}; // LoadDroppedFiles()

hookable_event_t frame    = {0};
/* uwaga uwaga: należy pamiętać o tym, żeby usuwać niepotrzebne hooki
   dla 'frame, bo bardzo zpowalniają rysowanie wszystkiego XDDD
   ~ kpm
*/

// cdddddr(cddddr(cddddr(cadr(ptr))))
// ~ kpm
pointer ncdr(int n, pointer x) {
  while (n--)
    x = cdr(x);

  return x;
}

struct hlist_el {
  char *nam;
  hookable_event_t *he;
};

static struct hlist_el hookable_events_list[] = {
  {"keypress",       &keypress},
  {"click",          &click},
  {"unclick",        &unclick},
  {"frame",          &frame},
  {"clock",          &clocke},
  {"log",            &loge},
  {"resize",         &resize},
  {"new",            &new},
  {"update",         &update},
  {"delete",         &delete},
  {"files-dropped",  &filesdropped},
};
static int n_hookable_events = sizeof(hookable_events_list)/
  sizeof(*hookable_events_list);

void do_hooks(hookable_event_t *he, pointer args)
{
  int i;
  if (!scheme_is_initialized)
    return;

  for (i = 0; i < he->n; ++i) {
    if (he->v[i])
      scheme_call(&scm, he->v[i], args);
  }
}

// Color → '(r g b)
// TODO: sprawdzić czy przypadkiem nie powinien zwracać też alpha
// (r g b a?)
// ~ kpm
static pointer color2list(scheme *sc, Color c)
{
  pointer r = mk_integer(sc, c.r),
    g = mk_integer(sc, c.g),
    b = mk_integer(sc, c.b);

  return Cons(r, Cons(g, Cons(b, sc->NIL)));
}

// '(r g b a?) → (Color){r, g, b, a}
static Color list2color(scheme *sc, pointer p)
{
  if (list_length(sc, p) != 3 && list_length(sc, p) != 4)
    return (Color) { 0xff, 0x00, 0xff, 0xff };
  // ty debilu nie podales koloru duze elo
  // ~ kpm

  Color c;
  c.r = rvalue(car(p)),
  c.g = rvalue(cadr(p)),
  c.b = rvalue(caddr(p)),
  c.a = cadddr(p) == sc->NIL ? 255 : rvalue(cadddr(p));

  return c;
}

void update_screen_size_variables(void)
{
  int w = GetScreenWidth(), h = GetScreenHeight();

  if (w != last_screen_width || h != last_screen_height) {
    last_screen_width = w, last_screen_height = h;
    do_hooks(&resize, cons(&scm, mk_integer(&scm, w),
                           cons(&scm, mk_integer(&scm, h), scm.NIL)));
  }
}

static pointer scm_get_all_of_thing(scheme *sc, int max, pointer(*f)(scheme*, pointer))
{
  pointer ret, cur;
  int i;

  if (max < 1)
    return sc->NIL;

  ret = cons(sc, f(sc, cons(sc, mk_integer(sc, 0), sc->NIL)),
             sc->NIL);
  cur = ret;

  for (i = 1; i < max; ++i) {
    set_cdr(cur, cons(sc, f(sc, cons(sc, mk_integer(sc, i), sc->NIL)), sc->NIL));
    cur = cdr(cur);
  }

  return ret;
}

static Rectangle normalize_rectangle(Rectangle r)
{
  if (r.width < 0) {
    r.width = -r.width;
    r.x = r.x - r.width;
  }
  if (r.height < 0) {
    r.height = -r.height;
    r.y = r.y - r.height;
  }

  return r;
}

static hookable_event_t *get_he_by_name(char *name)
{
  int i;
  for (i = 0; i < n_hookable_events; ++i)
    if (strcmp(hookable_events_list[i].nam, name) == 0)
      return hookable_events_list[i].he;

  return NULL;
}

static int get_real_n_hooks(hookable_event_t *he)
{
  int n = 0, i;
  for (i = 0; i < he->n; ++i)
    if (he->v[i])
      n++;

  return n;
}

Vector2 *list2poly(pointer l, int *n)
{
  *n = list_length(&scm, l);
  Vector2 *ret = malloc(sizeof(Vector2) * *n);
  int i;
  pointer cur = l, vec;

  for (i = 0; i < *n; ++i) {
    vec = car(cur);
    ret[i].x = rvalue(car(vec));
    ret[i].y = rvalue(cdr(vec));
    cur = cdr(cur);
  }

  return ret;
}

pointer poly2list(customb_data_t *cd)
{
  scheme *sc = &scm;
  pointer ret, cur;
  int i;

  if (cd->poly_pts < 1)
    return sc->NIL;

  ret = Cons(vec2cons(cd->poly[0]), sc->NIL);
  cur = ret;

  for (i = 1; i < cd->poly_pts; ++i) {
    set_cdr(cur, Cons(vec2cons(cd->poly[i]), sc->NIL));
    cur = cdr(cur);
  }

  return ret;
}

pointer fpl2list(FilePathList fpl)
{
  scheme *sc = &scm;
  pointer ret, cur;
  size_t i;

  if (fpl.count < 1)
    return sc->NIL;

  ret = Cons(mk_string(sc, fpl.paths[0]), sc->NIL);
  cur = ret;

  for (i = 1; i < fpl.count; ++i) {
    set_cdr(cur, Cons(mk_string(sc, fpl.paths[i]), sc->NIL));
    cur = cdr(cur);
  }

  return ret;
}

/* ---------- tu sie zaczyna implementacja scheme funkcji przeróżnych ------------ */

// (getenv env)
static pointer scm_getenv(scheme *sc, pointer args)
{
  expect_args("getenv", 1);
  char *s = string_value(car(args));
  char *v = getenv(s);
  if (!v) return mk_string(sc, "");
  return mk_string(sc, v);
}

// (white? color)
static pointer scm_white(scheme *sc, pointer args)
{
  expect_args("white?", 1);
  return is_white(list2color(sc, car(args))) ? sc->T : sc->F;
}

// (point-in-lens? pt lens-id)
static pointer scm_point_in_lens(scheme *sc, pointer args)
{
  extern Bounceables bounceables;
  Vector2 pt;
  int id;
  expect_args("point-in-lens?", 2);

  pt = cons2vec(car(args));
  id = (int)rvalue(cadr(args));

  if (id >= 0 && id < bounceables.n)
    if (bounceables.v[id].t == B_LENS)
      return collision_point_lens(pt, bounceables.v[id].data.lens) ? sc->T : sc->F;

  TraceLog(LOG_ERROR, "no such lens: %d", id);
  return sc->F;
}

// ups no troche jest to poplątane
// za późno wpadłem na to, że aktualizowanie listy od nowa przy każdym
// dodawaniu czegoś nie jest najlepszym pomysłem.
// więc przez to jest inny mechanizm dzizłania [*mirrors*, *customs*, *prisms*] i *sources*
// smh, słabo, ale nie mam siły tego naprawiać.
// to wychodzi od słabego początkowego założenia.
// jak teraz sobie myślę, to wszystko, co może zostać postawione przez użytkownika powinno być jednym
// typem jakies idk gowno_dupa_t, i mieć do wszystkiego takie same hooki, a teraz jak jest tak
// porozrzucane jak gnoj po polu to juz za pozno zeby naprawiac no bez jaj szkoda troche
//
// dlatego też NIE DODAM (delete-source) tutaj, tylko idk może kiedyś w scheme.
// teraz przyda mi się tylko usuwanie wszystkich, więc tylko to zeobie
// nie miałoby to sensu, bo nie ma żadnych hooków, które działałyby po usunięciu źródła,
// a dodawanie ich tylko pokomplikowałoby wszystko
// aaaa szkoda gadać,... szkoda strzępić ryjec

// (delete-all-sources)
static pointer scm_delete_all_sources(scheme *sc, pointer args)
{
  extern Sources sources;
  expect_args("delete-all-sources", 0);

  free(sources.v);
  sources = (Sources){0, 0, 0};
  return sc->T;
}


// (delete-source id)
/*
static pointer scm_delete_source(scheme *sc, pointer args)
{
  expect_args("delete-source", 1);
}
*/

// (point-in-triangle? pt center vert-len)
static pointer scm_point_in_triangle(scheme *sc, pointer args)
{
  Vector2 pt;
  prism_data_t pd = {0};

  expect_args("point-in-triangle?", 3);

  pt          = cons2vec(car(args));
  pd.center   = cons2vec(cadr(args));
  pd.vert_len = rvalue(caddr(args));
  calc_prism_pts(&pd);

  return CheckCollisionPointTriangle(pt, pd.p1, pd.p2, pd.p3) ? sc->T : sc->F;
}

// (vec-move-towards vec target max)
static pointer scm_vec_move_towards(scheme *sc, pointer args)
{
  Vector2 vec, targ;
  float max;

  expect_args("vec-move-towards", 3);

  vec  = cons2vec(car(args));
  targ = cons2vec(cadr(args));
  max  = rvalue(cadr(args));

  return vec2cons(Vector2MoveTowards(vec, targ, max));
}

// (normalize-angle a)
static pointer scm_normalize_angle(scheme *sc, pointer args)
{
  float a;
  expect_args("normalize-angle", 1);

  a = rvalue(car(args));
  return MR(normalize_angle(a));
}

// (point-in-line? pt p1 p2 threshold)
static pointer scm_point_in_line(scheme *sc, pointer args)
{
  Vector2 pt, p1, p2;
  int thr;

  expect_args("point-in-line?", 4);

  pt = cons2vec(car(args));
  p1 = cons2vec(cadr(args));
  p2 = cons2vec(caddr(args));
  thr = rvalue(cadddr(args));

  return CheckCollisionPointLine(pt, p1, p2, thr) ? sc->T : sc->F;
}

// (angle-between p1 p2)
static pointer scm_angle_between(scheme *sc, pointer args)
{
  Vector2 p1, p2;

  expect_args("angle-between", 2);

  p1 = cons2vec(car(args));
  p2 = cons2vec(cadr(args));

  return MR(Vector2Angle(p1, p2) * RAD2DEG);
}

// (normalize-rectangle rect)
static pointer scm_normalize_rectangle(scheme *sc, pointer args)
{
  float x, y, w, h;
  pointer l;
  Rectangle r;
  expect_args("normalize-rectangle", 1);

  l = car(args);
  x = rvalue(car(l));
  y = rvalue(cadr(l));
  w = rvalue(caddr(l));
  h = rvalue(cadddr(l));

  r = normalize_rectangle((Rectangle){x, y, w, h});
  return Cons(MR(r.x),
              Cons(MR(r.y),
                   Cons(MR(r.width),
                        Cons(MR(r.height), sc->NIL))));
}

// (create-lens center d r)
// XDDD center jako argument bo mój kod nie jest w stanie zrozumieć, że coś może być POD KĄTEM
// ~ kpm
static pointer scm_create_lens(scheme *sc, pointer args)
{
  extern Bounceables bounceables;
  Vector2 center;
  float d, r;

  expect_args("create-lens", 3);
  center = cons2vec(car(args));
  d      = rvalue(cadr(args));
  r      = rvalue(caddr(args));

  add_lens(center, d, r);

  do_hooks(&new, Cons(mk_symbol(sc, "lens"), Cons(MKI(bounceables.n-1), sc->NIL)));
  return MKI(bounceables.n-1);
}

// (create-prism '(x . y) vert-len n)
static pointer scm_create_prism(scheme *sc, pointer args)
{
  extern Bounceables bounceables;
  float n;
  int vert_len;
  Vector2 center;
  pointer center_s;

  expect_args("create-prism", 3);

  center_s = car(args);
  center.x = rvalue(car(center_s));
  center.y = rvalue(cdr(center_s));
  vert_len = rvalue(cadr(args));
  n        = rvalue(caddr(args));

  add_prism(center, vert_len, n);

  do_hooks(&new, Cons(mk_symbol(sc, "prism"), Cons(MKI(bounceables.n-1), sc->NIL)));
  return MKI(bounceables.n-1);
}

// (real-register-custom l f1 f2)
static pointer scm_register_custom(scheme *sc, pointer args)
{
  customb_data_t *cd = malloc(sizeof(customb_data_t));
  expect_args("real-register-custom", 3);

  cd->poly = list2poly(car(args), &cd->poly_pts), cd->draw = cadr(args), cd->remap = caddr(args);
  add_bounceable(B_CUSTOM, cd);

  return sc->T;
}

static pointer tB2sym(scheme *sc, bounceable_type_t t)
{
  switch (t) {
  case B_MIRROR: return mk_symbol(sc, "mirror");
  case B_PRISM: return mk_symbol(sc, "prism");
  case B_CUSTOM: return mk_symbol(sc, "custom");
  case B_LENS: return mk_symbol(sc, "lens");
  }

  return mk_symbol(sc, "INVALID");
}

// (delete-bounceable id)
static pointer scm_delete_bounceable(scheme *sc, pointer args)
{
  int id;
  extern Bounceables bounceables;

  expect_args("delete-bounceable", 1);
  id = rvalue(car(args));

  if (id >= 0 && id < bounceables.n) {
    bounceables.v[id].removed = 1;
    do_hooks(&delete, Cons(tB2sym(sc, bounceables.v[id].t), Cons(MKI(id), sc->NIL)));
    return sc->T;
  }

  TraceLog(LOG_ERROR, "no such bounceable: %d", id);
  return sc->F;
}

// (get-hook 'type id)
static pointer scm_get_hook(scheme *sc, pointer args)
{
  char *type;
  int id;
  hookable_event_t *he;
  expect_args("get-hook", 2);

  type = symname(car(args));
  id = rvalue(cadr(args));

  he = get_he_by_name(type);
  // chuj dupa biskupa
  // ~ kpm
  if (he) {
    if (id >= 0 && id < he->n) {
      if (he->v[id])
        return he->v[id];
      else
        TraceLog(LOG_ERROR, "no such lambda: %s @ %d", type, id);
    } else
      TraceLog(LOG_ERROR, "no such lambda: %s @ %d", type, id);
  } else
    TraceLog(LOG_ERROR, "no such hookable event: %s", type);

  return sc->F;
}

// (get-all-hooks 'type)
static pointer scm_get_all_hooks(scheme *sc, pointer args)
{
  pointer ret, cur;
  int i;
  char *type;
  hookable_event_t *he;
  expect_args("get-all-hooks", 1);

  type = symname(car(args));
  he = get_he_by_name(type);

  if (!he) {
    TraceLog(LOG_ERROR, "no such hookable event: %s", type);
    return sc->F;
  }

  ret = sc->NIL;
  for (i = 0; i < he->n; ++i) {
    if (he->v[i]) {
      if (ret == sc->NIL) {
        ret = Cons(he->v[i], sc->NIL);
        cur = ret;
      } else {
        set_cdr(cur, Cons(he->v[i], sc->NIL));
        cur = cdr(cur);
      }
    }
  }

  return ret;
}

// (set-cursor v)
static pointer scm_set_cursor(scheme *sc, pointer args)
{
  expect_args("set-cursor", 1);

  SetMouseCursor(rvalue(car(args)));
  return sc->T;
}

// (rect-collision r1 r2)
// nie mam pomysłu na ciekawszą nazwę lol
// ~ kpm
static pointer scm_rect_collision(scheme *sc, pointer args)
{
  Rectangle r1, r2, ret;
  pointer sr1, sr2;
  expect_args("rect-collision", 2);

  sr1 = car(args);
  sr2 = cadr(args);

  r1 = (Rectangle) {
    rvalue(car(sr1)), rvalue(cadr(sr1)),
    rvalue(caddr(sr1)), rvalue(cadddr(sr1))
  };

  r2 = (Rectangle) {
    rvalue(car(sr2)), rvalue(cadr(sr2)),
    rvalue(caddr(sr2)), rvalue(cadddr(sr2))
  };

  ret = GetCollisionRec(normalize_rectangle(r1), normalize_rectangle(r2));

  return Cons(MR(ret.x), Cons(MR(ret.y), Cons(MR(ret.width), Cons(MR(ret.height), sc->NIL))));
}

// (get-window-flag v) → #t | #f
static pointer scm_get_window_flag(scheme *sc, pointer args)
{
  int v;
  expect_args("get-window-flag", 1);

  v = rvalue(car(args));

  return IsWindowState(v) ? sc->T : sc->F;
}

// (set-window-flag v #t|#f) → nil
// TODO: ujednolicić window_flag i winconf
static pointer scm_set_window_flag(scheme *sc, pointer args)
{
  int v;
  bool b;
  expect_args("set-window-flag", 2);

  v = rvalue(car(args));
  b = cadr(args) == sc->T;

  (b ? SetWindowState : ClearWindowState)(v);

  return sc->NIL;
}

// (real-fill-rect x y w h color)
static pointer scm_fill_rect(scheme *sc, pointer args)
{
  int x, y, w, h;
  Color c;
  expect_args("real-fill-rect", 5);

  x = rvalue(car(args));
  y = rvalue(cadr(args));
  w = rvalue(caddr(args));
  h = rvalue(cadddr(args));
  c = list2color(sc, cadddr(cdr(args)));

  DrawRectangle(x, y, w, h, c);

  return sc->NIL;
}

// (real-tracelog T text)
static pointer scm_tracelog(scheme *sc, pointer args)
{
  int type;
  char *text;

  expect_args("real-tracelog", 2);

  type = rvalue(car(args));
  text = string_value(cadr(args));

  TraceLog(type, text);

  return sc->NIL;
}

// troche getto rozwiązanie, wytłumaczone w main.c
// ~ kpm
static pointer scm_get_winconf(scheme *sc, pointer args)
{
  expect_args("get-winconf", 0);

  return Cons(color2list(sc, winconf.bgcolor),
              Cons(color2list(sc, winconf.mirror_color),
                   Cons(mk_integer(sc, winconf.state),
                        Cons(color2list(sc, winconf.prism_outline_color), sc->NIL))));
}

// (set-winconf bgcolor mirror-color state prism-outline-color) + wiecej w przyszlosci
static pointer scm_set_winconf(scheme *sc, pointer args)
{
  Color bg, mirrorc, prismc;
  sim_state_t state;

  expect_args("set-winconf", 4);
  bg      = list2color(sc, car(args));
  mirrorc = list2color(sc, cadr(args));
  state   = rvalue(caddr(args));
  prismc  = list2color(sc, cadddr(args));

  winconf.bgcolor = bg;
  winconf.mirror_color = mirrorc;
  winconf.state = state;
  winconf.prism_outline_color = prismc;

  return sc->NIL;
}

// nie dziala, nie mam sily
// ~ kpm
#if 0
// (popen "command" "mode") → port
static pointer scm_popen(scheme *sc, pointer args)
{
  char *s, *mode;
  FILE *fp;

  expect_args("popen", 2);
  s = string_value(car(args));
  mode = string_value(cadr(args));

  fp = popen(s, mode);

  return port_from_file(sc, fp, 0);
}
#endif

// (get-screen-size) → (w . h)
static pointer scm_get_screen_size(scheme *sc, pointer args)
{
  expect_args("get-screen-size", 0);

  return cons(sc, mk_integer(sc, GetScreenWidth()), mk_integer(sc, GetScreenHeight()));
}

static pointer scm_time(scheme *sc, pointer args)
{
  expect_args("time", 0);

  return mk_integer(sc, time(NULL));
}

static pointer scm_time_since_init(scheme *sc, pointer args)
{
  expect_args("time-since-init", 0);

  return mk_real(sc, GetTime());
}

#define BUF_STEP 512
static pointer scm_system(scheme *sc, pointer args)
{
#ifndef PLAN9
  int bufsiz = 0, n;
  char *buf = NULL, *command;
  FILE *f;

  expect_args("system", 1);
  command = string_value(car(args));

  f = popen(command, "r");
  while (!feof(f)) {
    buf = realloc(buf, bufsiz + BUF_STEP);
    n = fread(buf + bufsiz, 1, BUF_STEP, f);
    buf[bufsiz + n] = 0;
    bufsiz += BUF_STEP;
  }
  pclose(f);

  return mk_string(sc, buf);
#else
  return sc->NIL;
#endif
}
#undef BUF_STEP

__attribute__((noreturn)) static pointer scm_exit(scheme *sc, pointer args)
{
  int status = 0;

  // nie robi expect_args()

  if (list_length(sc, args) > 0)
    status = rvalue(car(args));

  TraceLog(LOG_INFO, "exiting with status %d from scheme script.", status);

  exit(status);

  return sc->NIL;
}

// (loads s) → nil
static pointer scm_loads(scheme *sc, pointer args)
{
  char *s;

  expect_args("loads", 1);
  s = string_value(car(args));

  scheme_load_string(sc, s);
  return sc->NIL;
}

// (real-draw-text text x y sz spacing color) → #t
static pointer scm_draw_text(scheme *sc, pointer args)
{
  float x, y, sz, spacing;
  Color color;
  char *s;
  Font fnt;

  expect_args("real-draw-text", 6);

  s       = string_value(car(args));
  x       = rvalue(cadr(args));
  y       = rvalue(caddr(args));
  sz      = rvalue(cadddr(args));
  spacing = rvalue(cadddr(cdr(args)));
  color   = list2color(sc, cadddr(cddr(args)));

  fnt = get_font_with_size(sz);
  DrawTextEx(fnt, s, (Vector2){x,y}, sz, spacing, color);

  return sc->T;
}

// (real-measure-text text size spacing) → size
static pointer scm_measure_text(scheme *sc, pointer args)
{
  extern Font default_font;
  char *s;
  float sz, spacing;
  Vector2 ret;

  expect_args("measure-text", 3);

  s = string_value(car(args));
  sz = rvalue(cadr(args));
  spacing = rvalue(caddr(args));

  ret = MeasureTextEx(get_font_with_size(sz), s, sz, spacing);

  return cons(sc, mk_integer(sc, ret.x), mk_integer(sc, ret.y));
}

// (real-set-source! n x y angle thickness mouse-reactive n-beams color)
static pointer scm_set_source(scheme *sc, pointer args)
{
  extern Sources sources;
  Color color;
  source_t *s;

  int n, mouse_reactive, n_beams;
  float x, y, angle, thickness;

  expect_args("real-set-source!", 8);

  n              = rvalue(car(ncdr(0, args)));
  x              = rvalue(car(ncdr(1, args)));
  y              = rvalue(car(ncdr(2, args)));
  angle          = rvalue(car(ncdr(3, args)));
  thickness      = rvalue(car(ncdr(4, args)));
  mouse_reactive = car(ncdr(5, args)) == sc->T ? 1 : 0;
  n_beams        = rvalue(car(ncdr(6, args)));
  color          = list2color(sc, car(ncdr(7, args)));

  if (n >= sources.n) {
    TraceLog(LOG_WARNING, "no such source: %d", n);
    return sc->F;
  }

  s = &sources.v[n];
  if (n_beams >= s->size) {
    // i will not tracelog about that :3333
    n_beams = s->size - 1;
  }

  s->pt.x = x, s->pt.y = y, s->angle = angle;
  s->thickness = thickness, s->color = color;
  s->mouse_reactive = mouse_reactive;
  s->n_beam = n_beams;

  return sc->T;
}

// (real-get-source n) → ((x . y) angle thickness mouse-reactive n-beams '(r g b a))
static pointer scm_get_source(scheme *sc, pointer args)
{
  extern Sources sources;

  float n;
  source_t *s;

  expect_args("get-source", 1);
  n = rvalue(car(args));

  if (n > sources.n) {
    TraceLog(LOG_WARNING, "get-source: no such source: %d", n);
    return sc->F;
  }
  s = &sources.v[(int)n];

  return
    Cons(vec2cons(s->pt),
         Cons(MR(s->angle),
              Cons(MR(s->thickness),
                   Cons(s->mouse_reactive ? sc->T : sc->F,
                        Cons(mk_integer(sc, s->n_beam),
                             Cons(color2list(sc, s->color), sc->NIL))))));
}

// (get-all-sources) → '((id x y sz...) ...)
static pointer scm_get_all_sources(scheme *sc, pointer args)
{
  extern Sources sources;
  expect_args("get-all-sources", 0);

  return scm_get_all_of_thing(sc, sources.n, scm_get_source);
}

// (real-draw-line x1 y1 x2 y2 thickness r g b a) → nil
static pointer scm_draw_line(scheme *sc, pointer args)
{
  float x1, y1, x2, y2, thick;
  Color color;

  expect_args("real-draw-line", 6);

  x1    = rvalue(car(args));
  y1    = rvalue(cadr(args));
  x2    = rvalue(caddr(args));
  y2    = rvalue(cadddr(args));
  thick = rvalue(cadddr(cdr(args)));
  color = list2color(sc, cadddr(cddr(args)));

  DrawLineEx((Vector2){ x1, y1 }, (Vector2){ x2, y2 }, thick, color);

  return sc->NIL;
}

// (delete-hook sym id) → nil
static pointer scm_delete_hook(scheme *sc, pointer args)
{
  char *sym;
  int id;//, i;
  hookable_event_t *he;
  extern bool dont_tracelog;

  expect_args("delete-hook", 2);
  sym = symname(car(args));
  id = rvalue(cadr(args));

  he = get_he_by_name(sym);
  if (!he) {
    TraceLog(LOG_WARNING, "delete-hook: no such hookable event: %s", sym);
    return sc->F;
  }

  if (id > he->n) {
    TraceLog(LOG_ERROR, "cannot delete hook %d: no such hook", id);
    return sc->F;
  }

  he->v[id] = NULL;

  if (!dont_tracelog)
    fprintf(stderr, "(not tracelogged) successfully deleted hook %d for %s\n", id, sym);
  return sc->T;
}

// (real-add-hook 'type f) → #t | #f
static pointer scm_add_hook(scheme *sc, pointer args)
{
  extern bool dont_tracelog;
  char *name;
  pointer f;
  hookable_event_t *he;

  expect_args("real-add-hook", 2);

  name = symname(car(args));
  f = cadr(args);

  if (!is_closure(f)) {
    TraceLog(LOG_WARNING, "add-hook's 2nd arg has to be a function");
    return sc->F;
  }

  he = get_he_by_name(name);
  if (!he) {
    TraceLog(LOG_WARNING, "add-hook: no such hookable event: %s", name);
    return sc->F;
  }

  dyn_add_ptr_sized(he, f, sizeof(pointer));
  /* he->hooks = realloc(he->hooks, (1 + he->n_hooks) * sizeof(pointer)); */
  /* he->hooks[he->n_hooks] = f; */
  /* he->n_hooks++; */

  if (!dont_tracelog)
    fprintf(stderr, "(not tracelogged) successfully added hook %p for %s\n", (void*)f, name);
  return mk_integer(sc, he->n - 1);
}

// (get-mouse-position) → '(x y)
static pointer scm_get_mouse_position(scheme *sc, pointer args)
{
  Vector2 pos = GetMousePosition();
  (void)args;

  return cons(sc, mk_integer(sc, pos.x), mk_integer(sc, pos.y));
}

// (create-mirror x1 y1 x2 y2) → nil
static pointer scm_create_mirror(scheme *sc, pointer args)
{
  int x1, y1, x2, y2;
  extern Bounceables bounceables;

  expect_args("create-mirror", 4);

  x1 = rvalue(car(args));
  y1 = rvalue(cadr(args));
  x2 = rvalue(caddr(args));
  y2 = rvalue(cadddr(args));

  add_mirror((Vector2){x1,y1}, (Vector2){x2,y2});
  do_hooks(&new, Cons(mk_symbol(sc, "mirror"), Cons(MKI(bounceables.n-1), sc->NIL)));

  return MKI(bounceables.n-1);
}


// (get-bounceable id) → TYPE_data_t
static pointer scm_get_bounceable(scheme *sc, pointer args)
{
  int id;
  bounceable_t *cur;
  extern Bounceables bounceables;

  expect_args("get-bounceable", 1);

  id = rvalue(car(args));
  if (id >= 0 && id < bounceables.n) {
    cur = &bounceables.v[id];
    switch (cur->t) {
    case B_MIRROR: {
      Vector2 p1 = cur->data.mirror->p1, p2 = cur->data.mirror->p2;
      // ('mirror (x . y) (x . y))
      return Cons(mk_symbol(sc, "mirror"),
                  Cons(Cons(MKI(p1.x), MKI(p1.y)),
                       Cons(Cons(MKI(p2.x), MKI(p2.y)), sc->NIL)));
    } break;
    case B_PRISM: {
      prism_data_t *pd = cur->data.prism;
      Vector2 center = pd->center;

      // ('prism (center-x . center-y) (p1-x . p1-y) (p2-x . p2-y) (p3-x . p3-y) vert_len n)
      return Cons(mk_symbol(sc, "prism"),
                  Cons(vec2cons(center),
                       Cons(vec2cons(pd->p1),
                            Cons(vec2cons(pd->p2),
                                 Cons(vec2cons(pd->p3),
                                      Cons(MR(pd->vert_len),
                                           Cons(MR(pd->n), sc->NIL)))))));
    } break;
    case B_CUSTOM: {
      customb_data_t *cd = cur->data.custom;
      // ('custom (poly-pts) draw-function remap-function)
      return Cons(mk_symbol(sc, "custom"),
                  Cons(poly2list(cd),
                       Cons(cd->draw,
                            Cons(cd->remap, sc->NIL))));
    } break;
    case B_LENS: {
      lens_data_t *ld = cur->data.lens;

      // ('lens p1 p2 r center d)
      return Cons(mk_symbol(sc, "lens"),
                  Cons(vec2cons(ld->p1),
                       Cons(vec2cons(ld->p2),
                            Cons(MR(ld->r),
                                 Cons(vec2cons(ld->center),
                                      Cons(MR(ld->d),
                                           sc->NIL))))));
    } break;
    default: {
      abort();
      warnx("%s: %d not implemented for get-bounceable", __func__, cur->t);
    }
    }
  } else {
    TraceLog(LOG_ERROR, "no such bounceable: %d", id);
  }

  return sc->F;
}

// (set-mirror! id pt1 pt2)
static pointer scm_set_mirror(scheme *sc, pointer args)
{
  extern Bounceables bounceables;
  int id;
  Vector2 p1, p2;
  pointer sp1, sp2;

  expect_args("set-mirror!", 3);
  id  = rvalue(car(args));
  sp1 = cadr(args);
  sp2 = caddr(args);

  p1.x = rvalue(car(sp1)), p1.y = rvalue(cdr(sp1));
  p2.x = rvalue(car(sp2)), p2.y = rvalue(cdr(sp2));

  if (id >= 0 && id < bounceables.n) {
    if (bounceables.v[id].t == B_MIRROR) {
      bounceables.v[id].data.mirror->p1 = p1;
      bounceables.v[id].data.mirror->p2 = p2;

      // lol
      do_hooks(&update, Cons(mk_symbol(sc, "mirror"), Cons(MKI(id), sc->NIL)));
      return sc->T;
    } else {
      TraceLog(LOG_ERROR, "couldn't set-mirror! with id %d: not a mirror", id);
    }
  } else {
    TraceLog(LOG_ERROR, "couldn't set-mirror! with id %d: no such bounceable", id);
  }

  return sc->F;
}

// (set-lens! id center d r)
static pointer scm_set_lens(scheme *sc, pointer args)
{
  extern Bounceables bounceables;
  Vector2 center;
  float d, r;
  int id;

  expect_args("set-lens!", 4);
  id     = rvalue(car(args));
  center = cons2vec(cadr(args));
  d      = rvalue(caddr(args));
  r      = rvalue(cadddr(args));

  if (id >= 0 && id < bounceables.n) {
    if (bounceables.v[id].t == B_LENS) {
      bounceables.v[id].data.lens->center = center;
      bounceables.v[id].data.lens->r = r;
      bounceables.v[id].data.lens->d = d;
      calc_lens_stuff(bounceables.v[id].data.lens);

      do_hooks(&update, Cons(mk_symbol(sc, "lens"), Cons(MKI(id), sc->NIL)));
      return sc->T;
    } else {
      TraceLog(LOG_ERROR, "couldn't set-lens! with id %d: not a lens", id);
    }
  } else {
    TraceLog(LOG_ERROR, "couldn't set-lens! with id %d: no such bounceable", id);
  }

  return sc->F;
}

// (set-prism! id pt vert-len n)
static pointer scm_set_prism(scheme *sc, pointer args)
{
  extern Bounceables bounceables;
  int id;
  int vert_len;
  float n;
  Vector2 center;
  pointer scenter;

  id       = rvalue(car(args));
  scenter  = cadr(args);
  center.x = rvalue(car(scenter));
  center.y = rvalue(cdr(scenter));
  vert_len = rvalue(caddr(args));
  n        = rvalue(cadddr(args));

  if (id >= 0 && id < bounceables.n) {
    if (bounceables.v[id].t == B_PRISM) {
      bounceables.v[id].data.prism->center = center;
      bounceables.v[id].data.prism->vert_len = vert_len;
      bounceables.v[id].data.prism->n = n;
      calc_prism_pts(bounceables.v[id].data.prism);

      do_hooks(&update, Cons(mk_symbol(sc, "prism"), Cons(MKI(id), sc->NIL)));
      return sc->T;
    } else {
      TraceLog(LOG_ERROR, "couldn't set-prism! with id %d: not a prism", id);
    }
  } else {
    TraceLog(LOG_ERROR, "couldn't set-prism! with id %d: no such bounceable", id);
  }

  return sc->F;
}

/*
static pointer scm_get_all_bounceables(scheme *sc, pointer args)
{
  extern Bounceables bounceables;
  expect_args("get-all-bounceables", 0);

  return scm_get_all_of_thing(sc, bounceables.n, scm_get_bounceable);
}
*/

// (real-create-source x y sz angle thickness reactive n_beams color) → id
// reactive? to #t | #f
static pointer scm_create_source(scheme *sc, pointer args)
{
  extern Sources sources;
  float x, y, sz, thickness, angle;
  Color color;
  bool rel;
  int n_beams;

  expect_args("real-create-source", 8);

  x = rvalue(car(args));
  y = rvalue(cadr(args));
  sz = rvalue(caddr(args));
  angle = rvalue(cadddr(args));
  thickness = rvalue(cadddr(cdr(args)));
  rel = cadddr(cddr(args)) == sc->T;
  n_beams = rvalue(cadddr(cdddr(args)));
  color = list2color(sc, cadddr(cddddr(args)));

  source_t s;
  s.mouse_reactive = rel, s.size = sz, s.pt = (Vector2){x, y}, s.thickness = thickness,
  s.angle = angle, s.color = color, s.n_beam = n_beams;
  add_source(s);

  return MKI(sources.n-1);
}

void initialize_scheme(void)
{
  extern char tinyscheme_r5rs_scm[];

  scheme_init(&scm);
  TraceLog(LOG_INFO, "loaded tinyscheme");

  scheme_set_input_port_file(&scm, stdin);
  scheme_set_output_port_file(&scm, stdout);

  scheme_define(&scm, scm.global_env, mk_symbol(&scm, "*version*"),
    mk_string(&scm, "0.0"));

  load_scheme_cfunctions();
  load_compiled_scripts();

  scheme_is_initialized = true;
}

void load_rc(void)
{
#ifdef RCFILE
  FILE *rc = fopen("rc.scm", "r+");
  if (rc) {
    scheme_load_file(&scm, rc);
    fclose(rc);
  } else {
    TraceLog(LOG_WARNING, "could not load rc.scm, continuing.");
  }
#endif /* RCFILE */
}

pointer scheme_click_info(struct mouse_information_t *mi)
{
  return cons(&scm, mi->first_click ? scm.T : scm.F,
      cons(&scm, mi->left ? scm.T : scm.F,
        cons(&scm, mi->right ? scm.T : scm.F, scm.NIL)));
}

// --------- definicje eksportowanych FFI-funkcji ---------

static void load_scheme_cfunctions(void)
{
  //SCHEME_FF(scm_popen,              "popen"); // krzysztof napraw
  SCHEME_FF(scm_getenv,              "getenv");
  SCHEME_FF(scm_white,               "white?");
  SCHEME_FF(scm_point_in_lens,       "point-in-lens?");
  SCHEME_FF(scm_set_lens,            "set-lens!");
  SCHEME_FF(scm_create_lens,         "create-lens");
  SCHEME_FF(scm_delete_all_sources,  "real-delete-all-sources");
  SCHEME_FF(scm_point_in_triangle,   "point-in-triangle?");
  SCHEME_FF(scm_vec_move_towards,    "vec-move-towards");
  SCHEME_FF(scm_normalize_angle,     "normalize-angle");
  SCHEME_FF(scm_angle_between,       "angle-between");
  SCHEME_FF(scm_point_in_line,       "point-in-line?");
  SCHEME_FF(scm_normalize_rectangle, "normalize-rectangle");
  SCHEME_FF(scm_create_prism,        "create-prism");
  SCHEME_FF(scm_register_custom,     "real-register-custom");
  SCHEME_FF(scm_delete_bounceable,   "delete-bounceable");
  SCHEME_FF(scm_get_all_hooks,       "get-all-hooks");
  SCHEME_FF(scm_get_hook,            "get-hook");
  SCHEME_FF(scm_set_mirror,          "set-mirror!");
  SCHEME_FF(scm_set_prism,           "set-prism!");
  SCHEME_FF(scm_set_cursor,          "set-cursor");
  SCHEME_FF(scm_rect_collision,      "rect-collision");
  /* SCHEME_FF(scm_get_all_bounceables, "get-all-bounceables"); */
  SCHEME_FF(scm_get_bounceable,      "get-bounceable");
  SCHEME_FF(scm_get_window_flag,     "get-window-flag");
  SCHEME_FF(scm_set_window_flag,     "set-window-flag");
  SCHEME_FF(scm_fill_rect,           "real-fill-rect");
  SCHEME_FF(scm_tracelog,            "real-tracelog");
  SCHEME_FF(scm_set_winconf,         "set-winconf");
  SCHEME_FF(scm_get_winconf,         "get-winconf");
  SCHEME_FF(scm_get_screen_size,     "get-screen-size");
  SCHEME_FF(scm_time_since_init,     "time-since-init");
  SCHEME_FF(scm_time,                "time");
  SCHEME_FF(scm_system,              "system");
  SCHEME_FF(scm_exit,                "exit");
  SCHEME_FF(scm_loads,               "loads");
  SCHEME_FF(scm_delete_hook,         "delete-hook");
  SCHEME_FF(scm_measure_text,        "real-measure-text");
  SCHEME_FF(scm_draw_text,           "real-draw-text");
  SCHEME_FF(scm_set_source,          "real-set-source!");
  SCHEME_FF(scm_get_source,          "get-source");
  SCHEME_FF(scm_get_all_sources,     "get-all-sources");
  SCHEME_FF(scm_create_mirror,       "create-mirror");
  SCHEME_FF(scm_create_source,       "real-create-source");
  SCHEME_FF(scm_add_hook,            "real-add-hook");
  SCHEME_FF(scm_get_mouse_position,  "get-mouse-position");
  SCHEME_FF(scm_draw_line,           "real-draw-line");
}
