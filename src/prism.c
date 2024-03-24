#include "optyka.h"
#include "raylib.h"
#include "raymath.h"

#include <stdlib.h>
#include <float.h>
#include <stdint.h>

#define N0 1.f

bool is_white(Color c)
{
  int epsilon = 20;
  int min = 50;
  if ((c.r >= c.g - epsilon && c.r <= c.g + epsilon) &&
      (c.g >= c.b - epsilon && c.g <= c.b + epsilon) &&
      (c.r > min))
    return true;

  return false;
}

typedef struct {
  float len;
  Color c;
} len2color_t;

// https://www.partow.net/miscellaneous/colours.html
static len2color_t len2color_vs[44];
static int len2color_vs_n = 44;

static void init_len2color_vs(void)
{
  len2color_vs[0] = (len2color_t) { 700.000, (Color){255, 000, 000, 255} };
  len2color_vs[1] = (len2color_t) { 640.427, (Color){255, 031, 000, 255} };
  len2color_vs[2] = (len2color_t) { 633.790, (Color){255, 063, 000, 255} };
  len2color_vs[3] = (len2color_t) { 626.205, (Color){255,  95, 000, 255} };
  len2color_vs[4] = (len2color_t) { 617.938, (Color){255, 127, 000, 255} };
  len2color_vs[5] = (len2color_t) { 609.126, (Color){255, 159, 000, 255} };
  len2color_vs[6] = (len2color_t) { 599.855, (Color){255, 191, 000, 255} };
  len2color_vs[7] = (len2color_t) { 590.184, (Color){255, 223, 000, 255} };
  len2color_vs[8] = (len2color_t) { 580.159, (Color){255, 255, 000, 255} };
  len2color_vs[9] = (len2color_t) { 569.363, (Color){223, 255, 000, 255} };
  len2color_vs[10] = (len2color_t) { 558.936, (Color){191, 255, 000, 255} };
  len2color_vs[11] = (len2color_t) { 548.938, (Color){159, 255, 000, 255} };
  len2color_vs[12] = (len2color_t) { 539.431, (Color){127, 255, 000, 255} };
  len2color_vs[13] = (len2color_t) { 530.508, (Color){ 95, 255, 000, 255} };
  len2color_vs[14] = (len2color_t) { 522.313, (Color){063, 255, 000, 255} };
  len2color_vs[15] = (len2color_t) { 515.126, (Color){031, 255, 000, 255} };
  len2color_vs[16] = (len2color_t) { 510.028, (Color){000, 255, 000, 255} };
  len2color_vs[17] = (len2color_t) { 508.593, (Color){000, 255, 031, 255} };
  len2color_vs[18] = (len2color_t) { 506.550, (Color){000, 255, 063, 255} };
  len2color_vs[19] = (len2color_t) { 504.217, (Color){000, 255,  95, 255} };
  len2color_vs[20] = (len2color_t) { 501.673, (Color){000, 255, 127, 255} };
  len2color_vs[21] = (len2color_t) { 498.961, (Color){000, 255, 159, 255} };
  len2color_vs[22] = (len2color_t) { 496.109, (Color){000, 255, 191, 255} };
  len2color_vs[23] = (len2color_t) { 493.133, (Color){000, 255, 223, 255} };
  len2color_vs[24] = (len2color_t) { 490.049, (Color){000, 255, 255, 255} };
  len2color_vs[25] = (len2color_t) { 482.402, (Color){000, 223, 255, 255} };
  len2color_vs[26] = (len2color_t) { 474.954, (Color){000, 191, 255, 255} };
  len2color_vs[27] = (len2color_t) { 467.812, (Color){000, 159, 255, 255} };
  len2color_vs[28] = (len2color_t) { 461.022, (Color){000, 127, 255, 255} };
  len2color_vs[29] = (len2color_t) { 454.648, (Color){000,  95, 255, 255} };
  len2color_vs[30] = (len2color_t) { 448.795, (Color){000, 063, 255, 255} };
  len2color_vs[31] = (len2color_t) { 443.661, (Color){000, 031, 255, 255} };
  len2color_vs[32] = (len2color_t) { 440.020, (Color){000, 000, 255, 255} };
  len2color_vs[33] = (len2color_t) { 435.779, (Color){031, 000, 255, 255} };
  len2color_vs[34] = (len2color_t) { 429.652, (Color){063, 000, 255, 255} };
  len2color_vs[35] = (len2color_t) { 422.651, (Color){ 95, 000, 255, 255} };
  len2color_vs[36] = (len2color_t) { 417.397, (Color){127, 000, 255, 255} };
  len2color_vs[37] = (len2color_t) { 414.553, (Color){159, 000, 255, 255} };
  len2color_vs[38] = (len2color_t) { 412.223, (Color){191, 000, 255, 255} };
  len2color_vs[39] = (len2color_t) { 410.835, (Color){223, 000, 255, 255} };
  len2color_vs[40] = (len2color_t) { 409.196, (Color){255, 000, 255, 255} };
  len2color_vs[41] = (len2color_t) { 405.659, (Color){255, 000, 223, 255} };
  len2color_vs[42] = (len2color_t) { 402.805, (Color){255, 000, 191, 255} };
  len2color_vs[43] = (len2color_t) { 397.406, (Color){255, 000, 159, 255} };
}

// https://en.wikipedia.org/wiki/Type_punning
// pamięć to tylko bajty obok siebie
// a Color to struct z 4 bajtami obok siebie
// rgba
// \__/
//   \            4 * 8 =
//    -------------------vv
#define as_u32(c) (*(uint32_t*)&(c))

float color2wavelen(Color c)
{
  len2color_t *closest = &len2color_vs[0];
  float min = FLT_MAX, cur;
  int i;
  uint32_t lookup_color;

  lookup_color = as_u32(c);

  for (i = 0; i < len2color_vs_n; ++i) {
    cur = MAX(as_u32(len2color_vs[i].c), lookup_color)
      - MIN(as_u32(len2color_vs[i].c), lookup_color);

    if (cur < min)
      min = cur, closest = &len2color_vs[i];
  }

  return closest->len;
}

Color wavelen2rgb(float len)
{
  len2color_t *closest = &len2color_vs[0];
  float min = FLT_MAX, cur;
  int i;

  for (i = 0; i < len2color_vs_n; ++i) {
    cur = fabsf(len2color_vs[i].len - len);
    if (cur < min) {
      min = cur;
      closest = &len2color_vs[i];
    }
  }

  return closest->c;
}

void calc_prism_pts(prism_data_t *pd)
{
  float a = pd->vert_len,
    h = (sqrt(3) * a)/2.f,
    r = h/3;
  Vector2 S = pd->center;

  pd->p1 = (Vector2) {
    S.x,
    S.y - 2*r
  };

  pd->p3 = (Vector2) {
    S.x - a/2,
    S.y + r
  };

  pd->p2 = (Vector2) {
    S.x + a/2,
    S.y + r
  };
}

void draw_prism(bounceable_t *b)
{
  prism_data_t *pd = b->data.prism;
  extern struct window_conf_t winconf;

  DrawTriangleLines(pd->p1, pd->p2, pd->p3, winconf.prism_outline_color);
}

typedef struct {
  Vector2 p1, p2;
} Line;

static source_t prism_mk_source(void)
{
  source_t s;
  s.angle = 0,
  s.mouse_reactive = false,
  s.n_beam = 1,
  s.size = 1,
  s.thickness = 1;

  return s;
}

// stworzy src->thickness
// tylko gdy is_white(src->color)
static void prism_cast_colors(Vector2 prev, float first_ang, float fin_ang, source_t *src, prism_data_t *pd)
{
  int n = MAX(3, src->thickness);
  int i, step = (700 - 390) / n;
  Color cur_color;
  /* int dist = floor(src->thickness / 2.f); */
  int dist = floor(src->thickness / (float)n);
  float darken_by = 255.f - (src->color.r + src->color.g + src->color.b)/3.f;

  for (i = 0; i < n; ++i) {
    cur_color = wavelen2rgb(390 + i*step);
    source_t s = prism_mk_source();

    int sz = (((float)src->thickness/2)-((i-2)*dist))/sqrt(2);
    Vector2 rot = Vector2Rotate(vec(sz, sz), (-45 - 90 + first_ang) * PI/180);
    Vector2 pt = vec(prev.x + rot.x, prev.y + rot.y);
    Vector2 targ = create_target(pt, first_ang);

    targ = vec(targ.x - (prev.x - pt.x), targ.y - (prev.y - pt.y));

    Vector2 next = pt;

    int max_iter = 128;
    while (!CheckCollisionPointTriangle(next, pd->p1, pd->p2, pd->p3) && max_iter) {
      next = Vector2MoveTowards(next, targ, 1);
      max_iter--;
    }
    // niektóre części dużej wiązki nie są w pryzmacie XDD

    int sw = GetScreenWidth();
    int sh = GetScreenHeight();

    do {
      next = Vector2MoveTowards(next, targ, 2);
    } while (CheckCollisionPointTriangle(next, pd->p1, pd->p2, pd->p3)
             && next.x > 0.f
             && next.y > 0.f
             && next.x < sw
             && next.y < sh);

    cur_color.a = 255.f - darken_by;

    s.pt = next;
    s.color = cur_color;
    s.thickness = 1;
    s.target = create_target(next, fin_ang + i*0.2);

#ifdef DRAW_LINES_INSIDE
    DrawLineEx(pt, next, 2, dim_color(cur_color, 128));
#endif

    draw_light(&s);
  }
}

static float get_alpha(float hit_angle, float ang)
{
  float diff = normalize_angle(ang - hit_angle);
  if ((90.f - diff) > 90)
    return normalize_angle(360 - 90 - diff);

  return fabsf(90.f - diff);
}

Vector2 prism_create_target(bounceable_t *b, Vector2 cur, Vector2 next, struct _teleport *tp, source_t *src)
{
  prism_data_t *pd = b->data.prism;
  float phi = pd->phi;
  float n = pd->n;
  Line
    A = ((Line){pd->p1, pd->p2}),
    B = ((Line){pd->p2, pd->p3}),
    C = ((Line){pd->p3, pd->p1}),
    *hit;

  if (CheckCollisionPointTriangle(cur, pd->p1, pd->p2, pd->p3))
    return next;

  if (CheckCollisionPointLine(next, A.p1, A.p2, 2)) hit = &A;
  if (CheckCollisionPointLine(next, B.p1, B.p2, 2)) hit = &B;
  if (CheckCollisionPointLine(next, C.p1, C.p2, 2)) hit = &C;

#ifdef COLOR_HIT_PRISM_LINE
  DrawLineV(hit->p1, hit->p2, GREEN);
#endif /* COLOR_HIT_PRISM_LINE */

  float hit_ang  = normalize_angle(Vector2Angle(hit->p1, hit->p2) * RAD2DEG);
  float line_ang = normalize_angle(Vector2Angle(cur, next) * RAD2DEG);
  /* DrawLineV(next, create_target(next, hit_ang + 90), WHITE); */

  float alpha = get_alpha(hit_ang, line_ang);
  float beta = asinf((N0/n) * sinf(alpha*DEG2RAD))*RAD2DEG;
  float delta = (n - 1) * phi;

  Vector2 end_targ = create_target(next, normalize_angle(line_ang + alpha - beta));

  int sw = GetScreenWidth();
  int sh = GetScreenHeight();

  /* DrawCircleV(end_targ, 20, PINK); */
  tp->serio = true;
  tp->luzik = next;
  /* tp->luzik =  */

  /* DrawLineV(tp->luzik, create_target(tp->luzik, normalize_angle(line_ang + delta)), VIOLET); */
  /* return vec(0,0); */

  if (is_white(src->color)) {
    prism_cast_colors(next, normalize_angle(line_ang + alpha - beta),
                      normalize_angle(line_ang + delta), src, pd);

    tp->do_trzeciej_warstwy_piekla = true;
    return vec(0, 0);
  }

  tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);
  do {
    tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);
  } while (CheckCollisionPointTriangle(tp->luzik, pd->p1, pd->p2, pd->p3)
           && tp->luzik.x > 0.f
           && tp->luzik.y > 0.f
           && tp->luzik.x < sw
           && tp->luzik.y < sh);

  tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);

#ifdef DRAW_LINES_INSIDE
  DrawLineEx(next, tp->luzik, src->thickness, dim_color(src->color, 128));
#endif

  return create_target(tp->luzik, normalize_angle(line_ang + delta));
}

#define φ 60
void add_prism(Vector2 center, int vert_len, float n)
{
  prism_data_t *pd = malloc(sizeof(prism_data_t));

  init_len2color_vs();

  pd->center = center;
  pd->vert_len = vert_len;
  pd->phi = φ;
  pd->n = n;

  calc_prism_pts(pd);

  add_bounceable(B_PRISM, pd);
}
