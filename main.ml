let bind port =
  let server_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let address = Unix.inet_addr_of_string "127.0.0.1" in
  Unix.bind server_socket (ADDR_INET (address, port));
  server_socket

type host =
  | Error of string
  | Host of (string * string)
  | TUNNEL of (string * string)

let copy (input, output) =
  let buffer = Bytes.create 102400 in
  try
    while true do
      let len = Unix.read input buffer 0 102400 in
      if len > 0 then (
        Unix.write output buffer 0 len |> Printf.printf "write %d \n";
        Log.info (Bytes.to_string buffer))
    done
  with e ->
    Printexc.to_string e |> Printf.printf "with exception %s";
    Unix.shutdown input Unix.SHUTDOWN_ALL;
    Unix.shutdown output Unix.SHUTDOWN_ALL

let connect_remote host port =
  let client_sock = Unix.socket PF_INET SOCK_STREAM 0 in
  let hentry = Unix.gethostbyname host in
  Unix.connect client_sock
    (Unix.ADDR_INET (hentry.h_addr_list.(0), int_of_string port));
  client_sock

(* CONNECT wwww.badiu.com:443 http/1.1\r\n *)
(* GET http://www.baidu.com *)
let parse_first_line str =
  let first_line = List.nth (Str.split (Str.regexp "\r\n") str) 0 in
  Log.info first_line;
  let tmp =
    first_line |> Str.split (Str.regexp " ") |> fun list -> List.nth list 1
  in
  let r = Str.regexp "^CONNECT" in
  if Str.string_match r first_line 0 then
    let l = Str.split (Str.regexp ":") tmp in
    TUNNEL (List.nth l 0, List.nth l 1)
  else
    let r = Str.regexp "/+" in
    let tmp = Str.split r first_line |> fun list -> List.nth list 1 in
    tmp |> Str.split (Str.regexp ":") |> fun list ->
    if List.length list == 0 then Host (List.nth list 0, List.nth list 1)
    else Host (List.nth list 0, "80")

let () =
  let port = 8080 in
  let address = bind port in
  Unix.listen address 1000;
  while true do
    let client, _ = Unix.accept address in
    let buffer_size = 1024 in
    let buffer = Bytes.create buffer_size in
    let n = Unix.read client buffer 0 buffer_size in
    let host = parse_first_line (Bytes.to_string buffer) in
    match host with
    | TUNNEL (host, port) ->
        Log.info (Printf.sprintf "%s:%s" host port);
        let s = connect_remote host port in
        let res =
          "HTTP/1.1 200 Connection Established\r\nProxy-agent: golang\r\n\r\n"
        in
        let buf = Bytes.of_string res in
        ignore (Unix.write s buf 0 (String.length res));
        Thread.create copy (client, s) |> Thread.join
    | Host (host, port) ->
        Log.info (Printf.sprintf "%s:%s" host port);
        let s = connect_remote host port in
        ignore (Unix.write s buffer 0 n);
        Thread.create copy (s, client) |> Thread.join
    | Error s -> Printf.printf "%s" s
  done
