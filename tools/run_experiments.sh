#!/bin/bash
# Runs the improvement-1 and improvement-2 experiment grid of the spec.
# Primary condition (spec 0.5): 2-slice (360 px), 3.75x => target 24 blocks
# (24*96 / 360 = 6.4 bpp), table_num 4, rlc on, palette off (see REPORT).
set -e
ROOT=$(cd "$(dirname "$0")/.." && pwd)
EXP=$ROOT/codec_gm/_build/default/bin/exp.exe
BOOKS=$ROOT/codec_gm/_build/default/bin/build_books.exe
INC=$ROOT/inc/model
TEST=$ROOT/images/test
TRAIN=$ROOT/images/train
OUT=${OUT:-$ROOT/results}
mkdir -p "$OUT"
run() { local name=$1; shift; echo "== $name $*"; "$EXP" "$INC" "$TEST" "$OUT" --name "$name" "$@" > "$OUT/$name.log" 2>&1; tail -1 "$OUT/$name.log"; }

# ---- improvement 1: forced macro (rgbg) so every slice is a sample ----
run E1.0 --force-macro --m1 m1a --seg 20 --images
run E1.1 --force-macro --m1 m1b --seg 20 --images
run E1.2 --force-macro --m1 m1c --seg 20 --images
run E1.3_L12 --force-macro --m1 m1c --seg 12
run E1.3_L36 --force-macro --m1 m1c --seg 36
# normal arbiter: compare on the slices that really end in macro
run E1.4_m1a --m1 m1a --seg 20
run E1.4_m1b --m1 m1b --seg 20
run E1.4_m1c --m1 m1c --seg 20

# ---- improvement 2 ----
run E2.0 --structure serial --images                       # the model today (baseline)
run E2.0_palette --structure serial --palette on           # reference: baseline with palette on
run E2.0_trunk4 --structure trunk4 --images                # the spec's stated baseline
run E2.1 --structure a --images
run E2.3 --structure b --images

# offline codebooks from the TRAINING set (spec 5.3: train/test separate)
"$EXP" "$INC" "$TRAIN" "$OUT" --name train_a --structure a --dump-hist "$OUT/hist_a.txt" > "$OUT/train_a.log" 2>&1
"$BOOKS" "$OUT/hist_a.txt" "$OUT/books_a_lane3.txt" --merge lane3 > "$OUT/books_a_lane3.log"
"$EXP" "$INC" "$TRAIN" "$OUT" --name train_b --structure b --dump-hist "$OUT/hist_b.txt" > "$OUT/train_b.log" 2>&1
"$BOOKS" "$OUT/hist_b.txt" "$OUT/books_b_oddeven.txt" --merge oddeven > "$OUT/books_b_oddeven.log"
# shared-book reference built the same way (even/odd classes but on serial), to separate
# "better book" from "better structure"
"$EXP" "$INC" "$TRAIN" "$OUT" --name train_serial --structure serial --dump-hist "$OUT/hist_serial.txt" > "$OUT/train_serial.log" 2>&1
"$BOOKS" "$OUT/hist_serial.txt" "$OUT/books_serial_oddeven.txt" --merge oddeven > "$OUT/books_serial_oddeven.log"

run E2.2 --structure a --books lane3 --books-file "$OUT/books_a_lane3.txt"
run E2.4 --structure b --books oddeven --books-file "$OUT/books_b_oddeven.txt" --images
run E2.4_serial_oddeven --structure serial --books oddeven --books-file "$OUT/books_serial_oddeven.txt"

# ---- appendix: 1-slice and 4-slice at the same 6.4 bpp ----
for s in "1 48" "4 12"; do set -- $s
  run E2.6_s$1_serial --structure serial --slice $1 --target $2
  run E2.6_s$1_a      --structure a      --slice $1 --target $2
  run E2.6_s$1_b      --structure b      --slice $1 --target $2
done
echo done
