# proxy.ml
一个用 OCaml 实现的 http(s) 代理服务器, 极低的资源占用，只支持 Linux 和 Mac OSX 在内的 Unix.


![demo](demo.gif)

# 编译运行
```shell
dune install
dune build
dune exec ./main.exe
```
```
vagrant@archlinux proxy.ml (main) $ ./_build/default/main.exe --help
A http(s) proxy server
  -p Server listen port (default: 8080)
  -help  Display this list of options
  --help  Display this list of options
  ```