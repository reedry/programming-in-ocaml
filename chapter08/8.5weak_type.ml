let x = ref [];;
(* val x : '_weak1 list ref = {contents = []}
  弱い型として一時的に扱い、後で具体的な型を決定する
  2 :: !x の場合 int list ref, true :: !x の場合 bool list ref になる *)
