let bind port =
  let server_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let address = Unix.inet_addr_of_string "127.0.0.1" in
  Unix.bind server_socket (ADDR_INET (address, port));
  server_socket

let handle_tcp tcp =
  let s = "hello world" in
  let len = String.length s in
  let x = Unix.send tcp (Bytes.of_string s) 0 len [] in
  Printf.printf "send %d bytes" x;
  flush stdout

let () =
  let port = 1080 in
  let address = bind port in
  Unix.listen address 10;
  while true do
    let client_sock, _ = Unix.accept address in
    let thread = Thread.create handle_tcp client_sock in
    Thread.join thread
  done
