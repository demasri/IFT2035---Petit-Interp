#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>

int tty_fd = STDIN_FILENO;

void set_raw_tty() {

  struct termios new_termios;

  tcgetattr (tty_fd, &new_termios);

  new_termios.c_lflag &= ~(ICANON | ECHO | ECHOCTL);
  new_termios.c_iflag &= ~(IMAXBEL | ISTRIP | ICRNL | INLCR
                           | IGNCR | ICRNL | IXON | IXOFF);

  tcsetattr (tty_fd, TCSANOW, &new_termios);
}

char read_tty() {
  char buf;
  read(tty_fd, &buf, 1);
  return buf;
}

char *read_line() {

  int len = 0;
  char *line = malloc(len+1);
  char c;

  printf("_"); /* prompt for input */

  for (;;) {

    fflush(stdout); /* sync terminal output */

    c = read_tty();

    if (c == 13) break; /* return key? */

    if (c == 127) { /* delete key? */
      if (len > 0) {
        len--;
        printf("\b\b  \b\b_"); /* erase last character */
      }
    } else {
      line[len++] = c;             /* add character */
      line = realloc(line, len+1); /* resize buffer */
      printf("\b%c_", c);          /* show character */
    }
  }

  printf("\b \b\n");  /* remove prompt */
  line[len] = '\0';   /* terminate string */

  return line;
}

int main() {

  char *x;
  char *y;

  set_raw_tty();

  x = read_line();
  y = read_line();

  printf("somme = %d\n", atoi(x) + atoi(y));

  free(x);
  free(y);

  return 0;
}
