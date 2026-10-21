open Types

let pos_of ~q_order ~level ~sign =
  let max = Tables.level_max ~q_order in
  if level < 0 || level >= max then
    spec_violation "mapper: level %d outside 0..%d for q_order %d" level (max - 1) q_order;
  if sign || level = 0 then level else max - level

let map ~q_order ~level ~sign =
  let max = Tables.level_max ~q_order in
  let pos = pos_of ~q_order ~level ~sign in
  let mid = max / 2 in
  if pos < mid then 2 * pos else max - 2 * (pos - mid) - 1

let unmap ~q_order ~symbol =
  let max = Tables.level_max ~q_order in
  if symbol < 0 || symbol >= max then
    spec_violation "unmapper: symbol %d outside 0..%d for q_order %d" symbol (max - 1) q_order;
  if symbol land 1 = 0 then symbol lsr 1 else max - 1 - (symbol lsr 1)
