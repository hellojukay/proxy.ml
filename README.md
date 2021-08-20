# proxy.ml
一个用 OCaml 实现的 http(s) 代理服务器,暂不支持 Windows.

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