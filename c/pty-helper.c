/* The child half of spawning a terminal.

   posix_spawn is used instead of forkpty because fork() in a threaded process
   runs the runtime's pthread_atfork handlers in the child, and those take
   locks another thread may have been holding: the child then never execs and
   never dies. posix_spawn does not run them.

   posix_spawn has no hook for the one thing a terminal still needs, which is
   claiming the pty as the controlling terminal, so that happens here, in a
   process that is already a session leader with its stdio on the slave. */

#include <stdio.h>
#include <unistd.h>
#include <sys/ioctl.h>

int main(int argc, char **argv) {
    if (argc < 2) return 2;
    ioctl(STDIN_FILENO, TIOCSCTTY, 0);
    execl("/bin/sh", "/bin/sh", "-c", argv[1], (char *) NULL);
    return 127;
}
