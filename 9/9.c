#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "raylib.h"
#include "9.h"

extern char* argv0;
int screenw = 0, screenh = 0;

void
errx(int v, char *fmt, ...)
{
  va_list vl;
  va_start(vl, fmt);

  vfprintf(stderr, fmt, vl);
  fprintf(stderr, "\n");
  exit(v);
}

void
warnx(char *fmt, ...)
{
  va_list vl;
  va_start(vl, fmt);

  vfprintf(stderr, fmt, vl);
  fprintf(stderr, "\n");
}

void
TraceLog(int n, char *fmt, ...)
{
  va_list vl;
  va_start(vl, fmt);
  warnx(fmt, vl);
}


bool
IsFileDropped(void)
{
  return false;
}

void
DrawCircleLines(int cx, int cy, float radius, Color color)
{
}

void
UnloadDroppedFiles(FilePathList fpl)
{
}

void
SetMouseCursor(int c)
{
}

bool
IsWindowState(unsigned int fl)
{
  return true;
}

double
GetTime(void)
{
  return 0;
}

Vector2
MeasureTextEx(Font f, char *s, float size, float spacing)
{
  Vector2 v = {8*strlen(s),16};
  return v;
}

FilePathList
LoadDroppedFiles(void)
{
  FilePathList fpl;
  fpl.capacity = 0;
  fpl.count = 0;
  fpl.paths = NULL;

  return fpl;
}

short
isascii(char c)
{
  return c >= 0 && c <= 255;
}

void
SetTraceLogCallback(TraceLogCallback callback)
{
  warnx("ignoring settracelogcallback");
}

void
BeginScissorMode(int x, int y, int w, int h)
{

}

void
EndScissorMode(void)
{

}

int
GetScreenWidth(void)
{
  return screenw;
}

int
GetScreenHeight(void)
{
  return screenh;
}

Font
LoadFontFromMemory(char *ft, unsigned char *data, int size, int fontsize, int *fc, int gc)
{
  Font f;
  return f;
}


void
SetTextureFilter(Texture2D texture, int filter)
{
}

void*
GetWindowHandle(void)
{
  return NULL;
}



void
SetTargetFPS(int x)
{

}

void
SetExitKey(int k)
{

}

//static int ctr = 1000;
bool
WindowShouldClose(void)
{
  //ctr--;
  //return ctr == 0;
  return false;
}

Vector2 mpos;

Vector2
GetMousePosition(void)
{
  return mpos;
}

int cpressed;
int
GetCharPressed(void)
{
  return cpressed;
}

char *cpu8ret = NULL;

char*
CodepointToUTF8(int cp, int *sz)
{
  if (!cpu8ret) {
    cpu8ret = malloc(4);
    memset(cpu8ret, 0, 4);
  }
  return cpu8ret;
}

int
GetKeyPressed(void)
{
  return cpressed;
}

bool lbtn_p, rbtn_p;

bool
IsMouseButtonDown(int k)
{
  switch (k) {
  case MOUSE_BUTTON_LEFT: return lbtn_p;
  case MOUSE_BUTTON_RIGHT: return rbtn_p;
  case MOUSE_BUTTON_MIDDLE: return false;
  }
  return false;
}

void
ClearWindowState(unsigned flag)
{
}

void
SetWindowState(unsigned flag)
{
}

float
fmaxf(float a, float b)
{
  if (a > b)
    return a;
  return b;
}

float
fminf(float a, float b)
{
  if (a < b)
    return a;
  return b;
}

