#define __attribute__(x) 

short isascii(char);
int strcasecmp(char*, char*);

#define sqrtf  sqrt
#define sinf   sin
#define acosf  acos
#define floorf floor
#define fabsf  fabs
#define atan2f atan2
#define cosf   cos
#define asinf  asin

float fmaxf(float, float);
float fminf(float, float);

void errx(int, char *, ...);
void warnx(char*, ...);

