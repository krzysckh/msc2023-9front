#include <u.h>
#include <libc.h>

void
main(int argc, char **argv)
{
  if (argc != 2)
    sysfatal("usage: f2c filename");

  int f = open(argv[1], OREAD);
  char *newfname = strdup(argv[1]);
  int i = 0;
  while (1) {
    if (!newfname[i]) break;
    if (newfname[i] == '.' || newfname[i] == '/' || newfname[i] == '\\' || newfname[i] == '-') newfname[i] = '_';
    i++;
  }
  
  print("char %s[] = {\n", newfname);
  char buf[512];
  while (1) {
    int r = read(f, buf, 512);
    for (i = 0; i < r; ++i)
      print("%d, ", buf[i]);
    if (r == 0)
      break;
  }

  print("0};\n\n");

  close(f);
}