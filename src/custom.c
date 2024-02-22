#include "optyka.h"
#include "tinyscheme/scheme.h"

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

extern scheme scm;

void draw_custom(bounceable_t *b)
{
  customb_data_t *cd = b->data.custom;
  scheme_call(&scm, cd->draw, scm.NIL);
}

void custom_get_light_remap(customb_data_t *cd, Vector2 pt, float ang, struct _teleport *tp, float *ret_ang)
{
  scheme *sc = &scm;
  pointer ret = scheme_call(sc, cd->remap, Cons(Cons(MKI(pt.x), MKI(pt.y)), Cons(MKI(ang), sc->NIL)));
  if (list_length(sc, ret) != 2) {
    TraceLog(LOG_WARNING, "custom light_remapping at %p returned invalid values. ignoring.", cd->remap);
    *ret_ang = 0.f;
    tp->luzik = pt;
    tp->serio = false;
    return;
  }

  pointer sc_vec = car(ret);
  pointer sc_ang = cadr(ret);

  tp->serio = true;
  tp->luzik = (Vector2){rvalue(car(sc_vec)), rvalue(cdr(sc_vec))};
  *ret_ang = rvalue(sc_ang);
}
