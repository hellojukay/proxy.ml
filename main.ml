let bind port =
  let server_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let address = Unix.inet_addr_of_string "127.0.0.1" in
  Unix.bind server_socket (ADDR_INET (address, port));
  server_socket

type host = Error of string | Host of (string * int)

let parse_host line =
  ignore line;
  Host ("www.baidu.com", 80)

let parse_head head_string =
  let lines = String.split_on_char '\r' head_string in
  List.iter (fun line -> Printf.printf "%s" line) lines;
  flush stdout

let copy (input, output) =
  let buffer = Bytes.create 102400 in
  try
    while true do
      let len = Unix.read input buffer 0 102400 in
      Log.info
        (Printf.sprintf "write %d bytes to socket"
           (Unix.write output buffer 0 len))
    done
  with End_of_file -> Unix.shutdown input Unix.SHUTDOWN_ALL

let proxy tcp host port =
  let client_sock = Unix.socket PF_INET SOCK_STREAM 0 in
  let hentry = Unix.gethostbyname host in
  Unix.connect client_sock (Unix.ADDR_INET (hentry.h_addr_list.(0), port));
  let t1 = Thread.create copy (tcp, client_sock)
  and t2 = Thread.create copy (client_sock, tcp) in
  Thread.join t1;
  Thread.join t2

let read_tcp tcp =
  let buffer = Bytes.create (1024 * 10) in
  ignore (Unix.read tcp buffer 0 10240);
  let host = parse_host (Bytes.to_string buffer) in
  match host with
  | Error str -> Printf.printf "%s" str
  | Host (hostname, port) ->
      Log.info "proxy of www.baidu.com\n";
      proxy tcp hostname port

(**
1. 发送消息
2. 关闭读和写 
*)

let handle_tcp (client_tcp, _) =
  let thread = Thread.create read_tcp client_tcp in
  Thread.join thread;
  let s = "HTTP/1.1 200 Connection established\n\n" in
  let len = String.length s in
  let x = Unix.send client_tcp (Bytes.of_string s) 0 len [] in
  Printf.printf "send %d bytes\n" x;
  flush stdout
(* Unix.shutdown client_tcp Unix.SHUTDOWN_ALL *)

let () =
  let port = 1080 in
  let address = bind port in
  Unix.listen address 10;
  while true do
    let client_sock, client_addr = Unix.accept address in
    let thread = Thread.create handle_tcp (client_sock, client_addr) in
    Thread.join thread
  done
