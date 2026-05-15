@val external console: {..} = "console"

let () = {
  console["log"]("ReScript side: starting up")
  // wasm 側の print_endline は既に main.bc.wasm.js のロード時点で
  // 自動実行され、console に出力される
}