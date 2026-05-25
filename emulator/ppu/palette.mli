(** NES の 64 色マスターパレットと 4 色サブパレット。

    マスターパレット (64 色) は NES 本体の信号特性と画面表示色の対応で、
    実機の色は決まっているが、エミュレータごとに採用する RGB 値はかなり
    バリエーションがある (FCEUX, Nestopia, Mesen 等で微妙に違う)。
    本モジュールは「読み書き可能な 192 バイトの RGB triple」として
    マスターパレットを保持し、外部 .pal ファイル (FCEUX 互換 192 byte 形式)
    との相互変換を提供する。

    サブパレットは BG/sprite 用の 4 色組で、各要素はマスターインデックス
    (0..63) を指す。Pattern table viewer 等で「どの 4 色で表示するか」を
    決めるのに使う。実 PPU では palette RAM ($3F00-$3F1F) から動的に
    選ばれるが、本モジュールでは単なる int 配列として表現する。 *)

(** 64 色のマスターパレット。内部表現は 192 バイトの RGB triple (R,G,B,R,G,B,...)。
    [set_color] による in-place 編集に対応するため mutable。 *)
type t

(** 既定のマスターパレット。NesDev wiki の "PPU palettes" に掲載されている
    代表的な 2C02 値をベースとした内蔵デフォルト。呼び出しごとに新しい
    インスタンスを返す (mutable なので共有不可)。 *)
val default : unit -> t

(** マスターインデックス [idx] (0..63) の色を (r, g, b) で返す。各成分は 0..255。
    [idx] が範囲外なら [Invalid_argument]。 *)
val color : t -> int -> int * int * int

(** マスターインデックス [idx] (0..63) の色を上書きする。
    UI 側からの編集に使う。 [r/g/b] は 0..255 にクリップする。 *)
val set_color : t -> int -> r:int -> g:int -> b:int -> unit

(** [.pal] バイト列 (192 バイト) を読んで master を作る。
    長さが 192 でなければ [Error メッセージ] を返す。 *)
val of_pal_bytes : bytes -> (t, string) result

(** master を 192 バイトの [.pal] 形式 (R,G,B の 64 連) として書き出す。
    戻り値は新たに割り当てられた bytes (master とは独立)。 *)
val to_pal_bytes : t -> bytes

(** 4 色サブパレット。各要素はマスターインデックス (0..63)。
    必ず長さ 4 で扱う。 *)
type sub = int array

(** 既定のサブパレット [|0x0F; 0x00; 0x10; 0x30|] (黒/濃灰/中灰/白)。
    Pattern viewer のデバッグ用初期値として使う。 *)
val default_sub : unit -> sub

(** 2-bit pixel 配列 (各要素 0..3) を [sub] でマスターインデックスへ、
    さらに [master] で RGB へ展開し、RGBA バイト列 (α 常に 255) を返す。

    戻り値の長さは入力の 4 倍。 *)
val pixels_to_rgba : int array -> master:t -> sub:sub -> bytes
