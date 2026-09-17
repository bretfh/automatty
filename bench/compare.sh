#!/bin/sh
# What the same bytes cost somebody else's terminal.
#
# tmux runs detached, with no client attached to it: it is then doing what cl-vt
# does and no more -- read the pty, parse it, keep the grid and the scrollback,
# draw nothing. alacritty runs in a nested headless sway of its own, so it never
# touches the screen you are looking at, and it is drawing as well as parsing.
#
# Both are timed the same way cl-vt is: cat the corpus on a pty, and wait for
# the terminal to have read all of it. An empty run of each is measured too and
# taken off, because starting a terminal is not reading anything.

set -e
DIR=${BENCH_DIR:-/tmp/cl-vt-bench}
ROUNDS=${ROUNDS:-5}
SOCK=cl-vt-bench
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"; tmux -L $SOCK kill-server 2>/dev/null || true' EXIT

CORPORA="plain color redraw"

for name in $CORPORA; do
  test -f "$DIR/$name.txt" || { echo "no $DIR/$name.txt -- run make bench first"; exit 1; }
done

median () { sort -n | awk '{v[NR]=$1} END {print v[int((NR+1)/2)]}'; }

say () {
  # say <name> <file> <median seconds> <median empty seconds>
  awk -v n="$1" -v f="$2" -v s="$3" -v z="$4" 'BEGIN {
    b = 0; while ((getline line < f) > 0) b += length(line) + 1;
    t = s - z; if (t <= 0) t = s;
    printf "  %-10s %7.1f MB/s   %7.1f M chars/s   (%.2f s for %.1f MB)\n",
           n, b/1048576/t, b/1000000/t, t, b/1048576;
  }'
}

# --- tmux ------------------------------------------------------------------
cat > "$WORK/tmux.conf" <<EOF
set -g history-limit 10000
set -g status off
EOF

tmux_round () {
  s=$(date +%s.%N)
  tmux -L $SOCK -f "$WORK/tmux.conf" new-session -d -x 80 -y 24 "cat $1" 2>/dev/null
  while tmux -L $SOCK has-session 2>/dev/null; do sleep 0.01; done
  e=$(date +%s.%N)
  awk -v a="$s" -v b="$e" 'BEGIN{print b-a}'
}

echo
echo "tmux $(tmux -V | cut -d' ' -f2), detached, 80x24, history-limit 10000"
i=0; : > "$WORK/tmux-empty"
while [ $i -lt $ROUNDS ]; do tmux_round /dev/null >> "$WORK/tmux-empty"; i=$((i+1)); done
EMPTY=$(median < "$WORK/tmux-empty")
for name in $CORPORA; do
  i=0; : > "$WORK/tmux-$name"
  while [ $i -lt $ROUNDS ]; do tmux_round "$DIR/$name.txt" >> "$WORK/tmux-$name"; i=$((i+1)); done
  say "$name" "$DIR/$name.txt" "$(median < "$WORK/tmux-$name")" "$EMPTY"
done

# --- alacritty, in a headless compositor of its own ------------------------
command -v alacritty >/dev/null 2>&1 || { echo; echo "no alacritty here"; exit 0; }
command -v sway >/dev/null 2>&1 || { echo; echo "no sway here to put alacritty in"; exit 0; }

cat > "$WORK/inner.sh" <<EOF
#!/bin/sh
for name in $CORPORA empty; do
  f=$DIR/\$name.txt
  [ "\$name" = empty ] && f=/dev/null
  i=0
  while [ \$i -lt $ROUNDS ]; do
    s=\$(date +%s.%N)
    alacritty --config-file /dev/null \\
      -o window.dimensions.columns=80 -o window.dimensions.lines=24 \\
      -e sh -c "cat \$f" >/dev/null 2>&1
    e=\$(date +%s.%N)
    awk -v a="\$s" -v b="\$e" 'BEGIN{print b-a}' >> $WORK/alacritty-\$name
    i=\$((i+1))
  done
done
swaymsg exit >/dev/null 2>&1
EOF
chmod +x "$WORK/inner.sh"
cat > "$WORK/sway.conf" <<EOF
output HEADLESS-1 resolution 1920x1080
exec $WORK/inner.sh
EOF

echo
echo "alacritty $(alacritty --version | cut -d' ' -f2), in a headless sway, 80x24"
WLR_BACKENDS=headless sway -c "$WORK/sway.conf" >/dev/null 2>&1 || true
EMPTY=$(median < "$WORK/alacritty-empty")
for name in $CORPORA; do
  say "$name" "$DIR/$name.txt" "$(median < "$WORK/alacritty-$name")" "$EMPTY"
done
echo
