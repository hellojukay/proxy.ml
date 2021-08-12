let bind port =
  let server_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let address = Unix.inet_addr_of_string "127.0.0.1" in
  Unix.bind server_socket (ADDR_INET (address, port));
  server_socket

(**
1. 发送消息
2. 关闭读和写 
*)
let handle_tcp (client_tcp, _) =
  let s = "hello world" in
  let len = String.length s in
  let x = Unix.send client_tcp (Bytes.of_string s) 0 len [] in
  Printf.printf "send %d bytes\n" x;
  flush stdout;
  Unix.shutdown client_tcp Unix.SHUTDOWN_ALL

let () =
  let port = 1080 in
  let address = bind port in
  Unix.listen address 10;
  while true do
    let client_sock, client_addr = Unix.accept address in
    let thread = Thread.create handle_tcp (client_sock, client_addr) in
    Thread.join thread
  done
