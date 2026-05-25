(** NES 標準コントローラ ($4016 / $4017).

    {2 プロトコル}

    実機の標準コントローラはシフトレジスタ型:
    - CPU は $4016 への bit 0 write (= strobe) で latch を制御する。
      strobe が high の間は現在のボタン状態を継続的に latch、
      high → low の falling edge でその瞬間の状態を 8 bit シフトレジスタへ転送する.
    - その後 CPU は $4016 (P1) / $4017 (P2) を 8 回 read することで
      A → B → Select → Start → Up → Down → Left → Right の順に 1 bit ずつ取り出す.
    - 8 回を超えた read は 1 を返す (実機の open bus 模倣).

    {2 $4016 write は両コントローラに影響}

    $4016 write は P1/P2 共通のストローブラインを駆動するので、
    本実装は両コントローラに同じ strobe write を伝える運用を想定する
    (Nes 側で wire up). *)

(** ボタンの種類. ビット順は $4016 read で出てくる順. *)
type button =
  | A
  | B
  | Select
  | Start
  | Up
  | Down
  | Left
  | Right

(** コントローラ 1 個分の状態. *)
type t

(** 全ボタン解放 / strobe low / shift register 空 で生成する. *)
val mk : unit -> t

(** ボタンの押下状態を直接書き込む. UI 側 (keyboard / joypad / 自動入力 等)
    からの入口. *)
val set_button : t -> button -> bool -> unit

(** 押下状態 (true = 押されている) を取得する. デバッグ・テスト用. *)
val get_button : t -> button -> bool

(** すべてのボタンを解放状態にする (フォーカスロスト時など). *)
val release_all : t -> unit

(** $4016 (もしくは $4017) write. bit 0 のみ意味あり.
    high → low の falling edge で現在のボタン状態を latch する. *)
val write_strobe : t -> int -> unit

(** $4016 / $4017 read. 1 bit を下位に返す (上位は 0 を返すスタブ).
    strobe high 中は常に A button の現状態を返す.
    strobe low 中は latch されたシフトレジスタから順に取り出す.
    9 回目以降は 1 を返す. *)
val read : t -> int
