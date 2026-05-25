(** パターンテーブル (CHR) のデコード。

    NES の CHR メモリ (0x0000-0x1FFF, 計 8 KB) は 2 つのパターンテーブルから
    なる。各パターンテーブルは 4 KB = 256 タイルで、1 タイルは 8×8 pixel
    かつ 1 pixel あたり 2 bit (4 色パレットインデックス) で符号化される。

    1 タイル 16 バイトの内訳:
      - bytes 0-7 : 低位ビットプレーン (各 byte が 1 行)
      - bytes 8-15: 高位ビットプレーン
      pixel[y][x] = ((low[y]  >> (7-x)) & 1)
                  + ((high[y] >> (7-x)) & 1) << 1

    本モジュールは CHR バイト列を入力にとって 0..3 のパレットインデックス
    配列を返す純粋なデコーダ。ミラーリング・バンク切替・パレット選択は
    呼び出し側の責務とする。 *)

(** 8×8 ピクセル 1 枚分。row-major (length 64)、各要素は 0..3。 *)
type tile = int array

(** [chr] の [base_ofs] から 16 バイトを読んで 1 タイルにデコードする。
    [chr] の長さが [base_ofs + 16] に満たない場合は [Invalid_argument]。 *)
val decode_tile : chr:bytes -> base_ofs:int -> tile

(** 4 KB のパターンテーブル全体 (256 タイル) を 128×128 ピクセルマップに
    展開する。タイルは 16×16 グリッドに左上→右下の順で並ぶ。

    戻り値は length [128 * 128 = 16384]、row-major (画像座標と一致)。
    [chr] の長さが [table_ofs + 4096] に満たない場合は [Invalid_argument]。 *)
val decode_table : chr:bytes -> table_ofs:int -> int array

(** パレットインデックス (0..3) の配列を greyscale RGBA バイト列に変換する。
    後で本物の NES パレットを使う際は別関数を用意する想定。

    色対応: 0 → #000000 / 1 → #555555 / 2 → #AAAAAA / 3 → #FFFFFF
    α は常に 255。戻り値の長さは入力の 4 倍。 *)
val to_rgba_greyscale : int array -> bytes
