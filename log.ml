let format (tm : Unix.tm) =
  Printf.sprintf "%d-%d-%d %d:%d:%d" (1900 + tm.tm_year) tm.tm_mon tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let info str =
  let tm = Unix.time () |> Unix.localtime in
  let date_str = format tm in
  Printf.printf "%s %s\n" date_str str;
  flush stdout

let printf = Printf.printf
