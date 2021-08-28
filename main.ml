let bind port =
  let server_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let address = Unix.inet_addr_of_string "0.0.0.0" in
  Unix.bind server_socket (ADDR_INET (address, port));
  server_socket

type host =
  | Error of string
  | GET of (string * string)
  | CONNECT of (string * string)

let copy (input, output) =
  let buffer = Bytes.create 1024 in
  try
    while true do
      let len = Unix.read input buffer 0 1024 in
      if len > 0 then Unix.write output buffer 0 len |> ignore
      else Unix.sleepf 0.1
    done
  with
  | End_of_file as e ->
      e |> Printexc.to_string |> Log.error;
      flush stdout;
      Unix.shutdown input Unix.SHUTDOWN_RECEIVE;
      Unix.shutdown output Unix.SHUTDOWN_SEND;
      Thread.exit ()
  | e ->
      e |> Printexc.to_string |> Log.error;
      flush stdout;
      Thread.exit ()

let connect_remote host port =
  try
    let client_sock = Unix.socket PF_INET SOCK_STREAM 0 in
    let hentry = Unix.gethostbyname host in
    Unix.connect client_sock
      (Unix.ADDR_INET (hentry.h_addr_list.(0), int_of_string port));
    Some client_sock
  with e ->
    e |> Printexc.to_string |> Log.error;
    flush stdout;
    None

(* CONNECT wwww.badiu.com:443 http/1.1\r\n *)
(* GET http://www.baidu.com *)
let parse_first_line str =
  let first_line = List.nth (Str.split (Str.regexp "\r\n") str) 0 in
  first_line |> Log.info;
  let tmp =
    first_line |> Str.split (Str.regexp " ") |> fun list -> List.nth list 1
  in
  let r = Str.regexp "^CONNECT" in
  if Str.string_match r first_line 0 then
    let l = Str.split (Str.regexp ":") tmp in
    CONNECT (List.nth l 0, List.nth l 1)
  else
    let r = Str.regexp "/+" in
    let tmp = Str.split r first_line |> fun list -> List.nth list 1 in
    tmp |> Str.split (Str.regexp ":") |> fun list ->
    if List.length list == 0 then GET (List.nth list 0, List.nth list 1)
    else GET (List.nth list 0, "80")

let handle_socket client =
  try
    let buffer_size = 1024 in
    let buffer = Bytes.create buffer_size in
    let n = Unix.read client buffer 0 buffer_size in
    let host = parse_first_line (Bytes.to_string buffer) in
    match host with
    | CONNECT (host, port) -> (
        let s = connect_remote host port in
        match s with
        | Some s ->
            let res =
              "HTTP/1.1 200 Connection Established\r\n\
               Proxy-agent: golang\r\n\
               \r\n"
            in
            let buf = Bytes.of_string res in
            ignore (Unix.write client buf 0 (String.length res));
            Thread.create copy (client, s) |> ignore;
            Thread.create copy (s, client) |> ignore
        | None -> Thread.exit ())
    | GET (host, port) -> (
        let s = connect_remote host port in
        match s with
        | Some s ->
            ignore (Unix.write s buffer 0 n);
            Thread.create copy (s, client) |> ignore;
            Thread.create copy (client, s) |> ignore
        | None -> Thread.exit ())
    | Error str -> str |> Log.error
  with e ->
    e |> Printexc.to_string |> Log.error;
    flush stdout;
    Thread.exit ()

let () =
  Sys.signal Sys.sigpipe Sys.Signal_ignore |> ignore;
  let usage_msg = "A http(s) proxy server"
  and port = ref 1080
  and anon_fun _ = () in
  let speclist =
    [ ("-p", Arg.Set_int port, "Server listen port (default: 8080)") ]
  in
  Arg.parse speclist anon_fun usage_msg;
  let address = bind !port in
  Unix.listen address 1000;
  let logger = new Log.logger in
  Printf.sprintf "server runing on :%d" !port |> logger#info;
  while true do
    let client, _ = Unix.accept address in
    Thread.create handle_socket client |> ignore
  done
