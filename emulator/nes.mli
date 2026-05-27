open Famicaml_common.Nesint

(** カートリッジマッパーの読み書きを表す抽象型。 内部実装。バスからのアクセスを取り替えるための間接層。 *)
type mapper_io

(** NES 本体の状態。CPU・PPU・バス・WRAM・現在挿入中のカートリッジを保持する。 *)
type t =
  { mutable power : bool
  ; mutable cart : Rom.Cartridge.t option
  ; mutable mapper : mapper_io
  ; cpu : Cpu.t (** Per-cycle 6502 CPU. *)
  ; ppu : Ppu.t
  ; apu : Apu.t
  ; controller1 : Controller.t (** P1 標準コントローラ ($4016). *)
  ; controller2 : Controller.t (** P2 標準コントローラ ($4017 read). *)
  ; memory_bus : Bus.t
  ; wram : Bytes.t
    (** 2KB の CPU 内蔵 RAM。eject / reset / power_off / connect すべてで保持される。 *)
  ; mutable ith_nmi : uint16
  ; mutable ith_reset : uint16
  ; mutable ith_irq : uint16
  ; mutable dma_source : int option
    (** OAMDMA pending. [Some high] の間、次の {!tick} で
        [$XX00-$XXFF] を OAM に転送し CPU を 513/514 cycle stall させる. *)
  }

(** 電源 off・カートリッジなしの NES を生成する。 実機で言えば箱から出してきた直後の状態。 *)
val mk : unit -> t

(** iNES バイト列を解析し、対応マッパーなら NES に接続する。 WRAM や CPU レジスタは保持される (実機ではゲーム動作中の差し替えは
    クラッシュするが、本実装はモデル上許容する)。 *)
val connect : t -> bytes -> (unit, Rom.Ines.error) result

(** Cartridge.t を直接接続する。Ines.parse 経由ではなく、テストや オフライン解析パスから利用する。 *)
val connect_cartridge : t -> Rom.Cartridge.t -> unit

(** カートリッジを引き抜く。WRAM・CPU レジスタはそのまま保持され、 $8000-$FFFF のアクセスは open bus 相当 (0 を返す / 書き込み無視) となる。 *)
val eject : t -> unit

(** RESET ボタン相当。$FFFC ベクタを読み込んで CPU の PC にロードする。 WRAM・CPU レジスタ (PC 以外) は保持される。 カートリッジが挿さっていない場合はベクタが
    0 になる。 *)
val reset : t -> unit

(** 電源を入れ、続けて reset を行う。 *)
val power_on : t -> unit

(** 電源を切る。本モデルでは WRAM はモジュールが解放されるまで残る (現実の SRAM は短時間ならデータを保持するという挙動の近似)。 *)
val power_off : t -> unit

(** 現在の cart の battery-backed SRAM ($6000-$7FFF, 8KB) への直接参照を返す.
    SRAM を持たない mapper (NROM/CNROM/UNROM) は None. *)
val sram : t -> Bytes.t option

(** 提供された 8KB SRAM bytes を cart の prg_ram に書き込む (in-place).
    サイズ不正 (≠ 8192) や SRAM 無しなら false. *)
val load_sram : t -> Bytes.t -> bool

(** 1 CPU cycle 進める (= PPU 3 dot 進む)。PPU が vblank で nmi_request を
    立てた場合は CPU に転写する. *)
val tick : t -> unit

(** 次の vblank 開始 (= 1 フレーム完了) まで {!tick} し続ける。
    NTSC では約 29780 CPU cycle ≈ 89342 PPU dot を消費する. *)
val run_until_frame : t -> unit
