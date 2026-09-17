#define _GNU_SOURCE
#include <pty.h>
#include <spawn.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/wait.h>

extern char **environ;

#ifndef VT_PTY_HELPER
#define VT_PTY_HELPER "vt-pty-helper"
#endif

/* The environment the terminal runs in: this one, with TERM and COLORTERM
   said. Built here, in the parent, because the child of a spawn may not. */
static char **vt_pty_env(void) {
    static const char *set[] = { "TERM=xterm-256color", "COLORTERM=truecolor" };
    const int extra = (int) (sizeof set / sizeof set[0]);
    int n = 0;
    while (environ[n]) n++;

    char **env = malloc(sizeof(char *) * (size_t) (n + extra + 1));
    if (!env) return NULL;

    int out = 0;
    for (int i = 0; i < n; i++) {
        int shadowed = 0;
        for (int j = 0; j < extra; j++) {
            const char *eq = strchr(set[j], '=');
            size_t len = (size_t) (eq - set[j]) + 1;      /* including the = */
            if (strncmp(environ[i], set[j], len) == 0) shadowed = 1;
        }
        if (!shadowed) env[out++] = environ[i];
    }
    for (int j = 0; j < extra; j++) env[out++] = (char *) set[j];
    env[out] = NULL;
    return env;
}

/* Run CMD under /bin/sh on a new pseudo-terminal. Answers the master fd, or
   -1, and writes the child pid to PID_OUT.

   openpty here, then posix_spawn the helper with its stdio on the slave.
   Not forkpty: fork() in a threaded process runs the runtime's atfork
   handlers in the child, and those take locks another thread was holding, so
   the child never execs and never dies -- a terminal that opens, says
   nothing, and outlives the image that opened it. posix_spawn runs no atfork
   handler. */
int vt_pty_spawn(const char *cmd, const char *helper, int rows, int cols,
                 int *pid_out) {
    struct winsize ws = {0};
    ws.ws_row = (unsigned short) rows;
    ws.ws_col = (unsigned short) cols;

    int master = -1, slave = -1;
    if (openpty(&master, &slave, NULL, NULL, &ws) < 0) return -1;

    char **env = vt_pty_env();
    if (!env) { close(master); close(slave); return -1; }

    posix_spawn_file_actions_t actions;
    posix_spawnattr_t attr;
    posix_spawn_file_actions_init(&actions);
    posix_spawnattr_init(&attr);

    posix_spawn_file_actions_adddup2(&actions, slave, STDIN_FILENO);
    posix_spawn_file_actions_adddup2(&actions, slave, STDOUT_FILENO);
    posix_spawn_file_actions_adddup2(&actions, slave, STDERR_FILENO);
    posix_spawn_file_actions_addclose(&actions, master);
    if (slave > STDERR_FILENO) posix_spawn_file_actions_addclose(&actions, slave);

    /* a session of its own, so the helper can claim the pty as its ctty */
    posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETSID);

    if (!helper || !*helper) helper = VT_PTY_HELPER;

    char *argv[3];
    argv[0] = (char *) helper;
    argv[1] = (char *) cmd;
    argv[2] = NULL;

    pid_t pid = 0;
    int rc = posix_spawn(&pid, helper, &actions, &attr, argv, env);

    posix_spawn_file_actions_destroy(&actions);
    posix_spawnattr_destroy(&attr);
    free(env);
    close(slave);

    if (rc != 0) { close(master); return -1; }
    *pid_out = (int) pid;
    return master;
}

void vt_pty_set_size(int fd, int rows, int cols) {
    struct winsize ws = {0};
    ws.ws_row = (unsigned short) rows;
    ws.ws_col = (unsigned short) cols;
    ioctl(fd, TIOCSWINSZ, &ws);
}
