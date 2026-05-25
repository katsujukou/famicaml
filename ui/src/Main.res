/* ---- 外部 JS バインディング ---- */

type uint8Array
type arrayBuffer
@new external newUint8Array: arrayBuffer => uint8Array = "Uint8Array"
@get external uint8ArrayLength: uint8Array => int = "length"
@get_index external uint8At: (uint8Array, int) => int = ""

type file
@send external fileArrayBuffer: file => promise<arrayBuffer> = "arrayBuffer"
@get external fileName: file => string = "name"

type intervalId
@val external setInterval: (unit => unit, int) => intervalId = "setInterval"
@val external clearInterval: intervalId => unit = "clearInterval"

type fileList
@get external fileListLength: fileList => int = "length"
@get_index external fileListItem: (fileList, int) => file = ""

@get external targetFiles: {..} => Nullable.t<fileList> = "files"

/* ---- Canvas / ImageData バインディング ---- */

type uint8ClampedArray
@new
external newUint8ClampedArrayFromArr: uint8Array => uint8ClampedArray = "Uint8ClampedArray"

type imageData
@new external newImageData: (uint8ClampedArray, int, int) => imageData = "ImageData"

type ctx2d
@send external putImageData: (ctx2d, imageData, int, int) => unit = "putImageData"
@send external clearRect: (ctx2d, int, int, int, int) => unit = "clearRect"
@set external setFillStyle: (ctx2d, string) => unit = "fillStyle"
@send external fillRect: (ctx2d, int, int, int, int) => unit = "fillRect"

type htmlCanvasElement
@send external getContext2d: (htmlCanvasElement, @as("2d") _) => ctx2d = "getContext"

external asCanvas: Dom.element => htmlCanvasElement = "%identity"

/* ---- .pal ダウンロード/アップロード ---- */

type blob
@new external newBlob: (array<uint8Array>, {..}) => blob = "Blob"
@val @scope("URL") external createObjectURL: blob => string = "createObjectURL"
@val @scope("URL") external revokeObjectURL: string => unit = "revokeObjectURL"

type anchorElement
@val @scope("document") external createAnchor: (@as("a") _) => anchorElement = "createElement"
@set external anchorHref: (anchorElement, string) => unit = "href"
@set external anchorDownload: (anchorElement, string) => unit = "download"
@send external anchorClick: anchorElement => unit = "click"

/* ---- wasm/main.ml の Js.export "FamiCaml" API ---- */

type cartInfo = {
  mapper: string,
  mirroring: string,
  hasBattery: bool,
  hasTrainer: bool,
  prgSize: int,
  chrSize: int,
}

type nesState = {
  power: bool,
  cart: Nullable.t<cartInfo>,
  resetVector: int,
  nmiVector: int,
  irqVector: int,
  pc: int,
}

type loadResult = {
  ok: bool,
  state: Nullable.t<nesState>,
  error: Nullable.t<string>,
}

type patternImage = {
  width: int,
  height: int,
  rgba: uint8Array,
}

type famiCamlApi = {
  loadRom: uint8Array => loadResult,
  eject: unit => unit,
  reset: unit => unit,
  powerOn: unit => unit,
  powerOff: unit => unit,
  state: unit => nesState,
  patternTable: int => Nullable.t<patternImage>,
  getMasterPalette: unit => uint8Array,
  setMasterPalette: uint8Array => bool,
  resetMasterPalette: unit => unit,
  setMasterColor: (int, int, int, int) => unit,
  getViewerSub: unit => uint8Array,
  setViewerSubSlot: (int, int) => unit,
}

@val @scope("globalThis") external famiCaml: Nullable.t<famiCamlApi> = "FamiCaml"

/* ---- ヘルパー ---- */

let kbStr = bytes => Int.toString(bytes / 1024) ++ " KB"

let hex16 = n => {
  let s = Int.toString(n, ~radix=16)->String.toUpperCase
  let pad = Math.Int.max(0, 4 - String.length(s))
  "$" ++ String.repeat("0", pad) ++ s
}

let hex8 = n => {
  let s = Int.toString(n, ~radix=16)->String.toUpperCase
  let pad = Math.Int.max(0, 2 - String.length(s))
  "$" ++ String.repeat("0", pad) ++ s
}

let toHex2 = n => {
  let s = Int.toString(n, ~radix=16)->String.toUpperCase
  String.length(s) == 1 ? "0" ++ s : s
}

let cssRgb = (r, g, b) =>
  "rgb(" ++ Int.toString(r) ++ "," ++ Int.toString(g) ++ "," ++ Int.toString(b) ++ ")"

let cssHex = (r, g, b) => "#" ++ toHex2(r) ++ toHex2(g) ++ toHex2(b)

/* "#rrggbb" を (r,g,b) に分解 */
let parseHexColor = s =>
  if String.length(s) == 7 {
    let r = Int.fromString(~radix=16, String.substring(s, ~start=1, ~end=3))
    let g = Int.fromString(~radix=16, String.substring(s, ~start=3, ~end=5))
    let b = Int.fromString(~radix=16, String.substring(s, ~start=5, ~end=7))
    switch (r, g, b) {
    | (Some(r), Some(g), Some(b)) => Some((r, g, b))
    | _ => None
    }
  } else {
    None
  }

/* master uint8Array (192 byte) から色 idx (0..63) の RGB を取り出す */
let readMaster = (master: uint8Array, idx) => {
  let off = idx * 3
  (uint8At(master, off), uint8At(master, off + 1), uint8At(master, off + 2))
}

/* canvas に pattern table を描画 (cart 未挿入時はグレーアウト). */
let renderPatternTable = (ref_: React.ref<Nullable.t<Dom.element>>, idx: int) => {
  switch (Nullable.toOption(famiCaml), ref_.current->Nullable.toOption) {
  | (Some(api), Some(elt)) =>
    let canvas = asCanvas(elt)
    let ctx = getContext2d(canvas)
    switch Nullable.toOption(api.patternTable(idx)) {
    | Some(img) =>
      let clamped = newUint8ClampedArrayFromArr(img.rgba)
      let imgData = newImageData(clamped, img.width, img.height)
      putImageData(ctx, imgData, 0, 0)
    | None =>
      clearRect(ctx, 0, 0, 128, 128)
      setFillStyle(ctx, "#222")
      fillRect(ctx, 0, 0, 128, 128)
    }
  | _ => ()
  }
}

let downloadPal = (bytes: uint8Array) => {
  let blob = newBlob([bytes], {"type": "application/octet-stream"})
  let url = createObjectURL(blob)
  let a = createAnchor()
  anchorHref(a, url)
  anchorDownload(a, "famicaml.pal")
  anchorClick(a)
  revokeObjectURL(url)
}

/* ---- Swatch コンポーネント ---- */

module Swatch = {
  @react.component
  let make = (
    ~r: int,
    ~g: int,
    ~b: int,
    ~size: int=24,
    ~selected: bool=false,
    ~title: string="",
    ~label: option<string>=?,
    ~onClick: option<ReactEvent.Mouse.t => unit>=?,
  ) => {
    let cursor = switch onClick {
    | Some(_) => "pointer"
    | None => "default"
    }
    /* 背景の明度に応じてラベル色を反転 (Y' = 0.299R + 0.587G + 0.114B) */
    let luminance = (r * 299 + g * 587 + b * 114) / 1000
    let textColor = luminance > 128 ? "#000" : "#fff"
    let shadowColor =
      luminance > 128 ? "rgba(255,255,255,0.6)" : "rgba(0,0,0,0.6)"
    let style: JsxDOMStyle.t = {
      width: Int.toString(size) ++ "px",
      height: Int.toString(size) ++ "px",
      backgroundColor: cssRgb(r, g, b),
      border: selected ? "2px solid #ff0" : "1px solid #444",
      cursor,
      display: "inline-flex",
      alignItems: "center",
      justifyContent: "center",
      fontFamily: "monospace",
      fontSize: "0.6rem",
      color: textColor,
      textShadow: "0 0 2px " ++ shadowColor,
      userSelect: "none",
      boxSizing: "border-box",
    }
    let content = switch label {
    | Some(s) => React.string(s)
    | None => React.null
    }
    switch onClick {
    | Some(handler) => <div style title onClick=handler> {content} </div>
    | None => <div style title> {content} </div>
    }
  }
}

/* ---- コンポーネント ---- */

module App = {
  @react.component
  let make = () => {
    let (state, setState) = React.useState((): option<nesState> => None)
    let (lastError, setLastError) = React.useState((): option<string> => None)
    let (selectedName, setSelectedName) = React.useState(() => "")
    let canvasLeft = React.useRef(Nullable.null)
    let canvasRight = React.useRef(Nullable.null)

    /* Palette 状態。wasm 側の真値をミラーする。
       UI からの編集はまず wasm を呼んでから state に reflect する. */
    let initialMaster = switch Nullable.toOption(famiCaml) {
    | Some(api) => api.getMasterPalette()
    | None => newUint8Array(%raw(`new ArrayBuffer(192)`))
    }
    let initialSub = switch Nullable.toOption(famiCaml) {
    | Some(api) => api.getViewerSub()
    | None => newUint8Array(%raw(`new ArrayBuffer(4)`))
    }
    let (master, setMaster) = React.useState(() => initialMaster)
    let (sub, setSub) = React.useState(() => initialSub)
    let (activeSlot, setActiveSlot) = React.useState(() => 0)
    let (showCode, setShowCode) = React.useState(() => false)

    /* wasm_of_ocaml は wasm モジュールを非同期に取得・初期化するため、
       React の初回 mount より遅く globalThis.FamiCaml が立つことがある。
       その場合 initialMaster/initialSub は zero-fill になっているので、
       wasm が準備でき次第 polling で master/sub をデフォルト値に差し替える。 */
    React.useEffect0(() => {
      switch Nullable.toOption(famiCaml) {
      | Some(_) =>
        /* 初回 render で既に取れているので polling 不要 */
        None
      | None =>
        let intervalRef = ref(None)
        let i = setInterval(() => {
          switch Nullable.toOption(famiCaml) {
          | Some(api) =>
            setMaster(_ => api.getMasterPalette())
            setSub(_ => api.getViewerSub())
            switch intervalRef.contents {
            | Some(i) => clearInterval(i)
            | None => ()
            }
          | None => ()
          }
        }, 50)
        intervalRef := Some(i)
        Some(() => clearInterval(i))
      }
    })

    let refreshPalette = () =>
      switch Nullable.toOption(famiCaml) {
      | Some(api) =>
        setMaster(_ => api.getMasterPalette())
        setSub(_ => api.getViewerSub())
      | None => ()
      }

    /* state / master / sub のどれが変わっても pattern table を再描画. */
    React.useEffect3(() => {
      renderPatternTable(canvasLeft, 0)
      renderPatternTable(canvasRight, 1)
      None
    }, (state, master, sub))

    let withApi = (f: famiCamlApi => unit) =>
      switch Nullable.toOption(famiCaml) {
      | Some(api) =>
        f(api)
        setState(_ => Some(api.state()))
      | None => setLastError(_ => Some("WASM module not yet loaded"))
      }

    let handleArrayBuffer = (api, buf) => {
      let arr = newUint8Array(buf)
      let r = api.loadRom(arr)
      if r.ok {
        setLastError(_ => None)
        setState(_ => Nullable.toOption(r.state))
      } else {
        setLastError(_ => Nullable.toOption(r.error))
      }
    }

    let onRomChange = event => {
      let target = ReactEvent.Form.target(event)
      switch Nullable.toOption(targetFiles(target)) {
      | Some(fs) if fileListLength(fs) > 0 =>
        let f = fileListItem(fs, 0)
        setSelectedName(_ => fileName(f))
        switch Nullable.toOption(famiCaml) {
        | None => setLastError(_ => Some("WASM module not yet loaded"))
        | Some(api) =>
          fileArrayBuffer(f)
          ->Promise.thenResolve(buf => handleArrayBuffer(api, buf))
          ->ignore
        }
      | _ => ()
      }
    }

    /* .pal ファイル import */
    let onPalChange = event => {
      let target = ReactEvent.Form.target(event)
      switch (Nullable.toOption(targetFiles(target)), Nullable.toOption(famiCaml)) {
      | (Some(fs), Some(api)) if fileListLength(fs) > 0 =>
        let f = fileListItem(fs, 0)
        fileArrayBuffer(f)
        ->Promise.thenResolve(buf => {
          let arr = newUint8Array(buf)
          if api.setMasterPalette(arr) {
            setLastError(_ => None)
            refreshPalette()
          } else {
            setLastError(_ =>
              Some(".pal の読み込みに失敗 (size = " ++ Int.toString(uint8ArrayLength(arr)) ++ " byte, expected 192)")
            )
          }
        })
        ->ignore
      | _ => ()
      }
    }

    let onEject = _ => withApi(api => api.eject())
    let onReset = _ => withApi(api => api.reset())
    let onPowerOn = _ => withApi(api => api.powerOn())
    let onPowerOff = _ => withApi(api => api.powerOff())

    let onExportPal = _ =>
      switch Nullable.toOption(famiCaml) {
      | Some(api) => downloadPal(api.getMasterPalette())
      | None => ()
      }

    let onResetPal = _ =>
      switch Nullable.toOption(famiCaml) {
      | Some(api) =>
        api.resetMasterPalette()
        refreshPalette()
      | None => ()
      }

    /* sub slot クリック → 編集対象に */
    let onSubSlotClick = slot => _ => setActiveSlot(_ => slot)

    /* master 色クリック → activeSlot にアサイン */
    let onMasterClick = idx => _ =>
      switch Nullable.toOption(famiCaml) {
      | Some(api) =>
        api.setViewerSubSlot(activeSlot, idx)
        setSub(_ => api.getViewerSub())
      | None => ()
      }

    /* color picker で activeSlot に対応する master 色を編集 */
    let activeMasterIdx = uint8ArrayLength(sub) >= 4 ? uint8At(sub, activeSlot) : 0
    let (ar, ag, ab) =
      uint8ArrayLength(master) >= 192 ? readMaster(master, activeMasterIdx) : (0, 0, 0)

    let onColorPickerChange = event => {
      let target = ReactEvent.Form.target(event)
      let value: string = target["value"]
      switch (parseHexColor(value), Nullable.toOption(famiCaml)) {
      | (Some((r, g, b)), Some(api)) =>
        api.setMasterColor(activeMasterIdx, r, g, b)
        refreshPalette()
      | _ => ()
      }
    }

    let renderRow = (label, value) =>
      <>
        <dt> {React.string(label)} </dt> <dd> {React.string(value)} </dd>
      </>

    let cartSection = (cart: cartInfo) =>
      <dl style={{marginTop: "0.5rem"}}>
        {renderRow("Mapper", cart.mapper)}
        {renderRow("Mirroring", cart.mirroring)}
        {renderRow("Battery", cart.hasBattery ? "yes" : "no")}
        {renderRow("Trainer", cart.hasTrainer ? "yes" : "no")}
        {renderRow("PRG", kbStr(cart.prgSize))}
        {renderRow("CHR", kbStr(cart.chrSize))}
      </dl>

    let stateSection = (s: nesState) =>
      <>
        <dl style={{marginTop: "0.5rem"}}>
          {renderRow("Power", s.power ? "ON" : "OFF")}
          {renderRow("PC", hex16(s.pc))}
          {renderRow("RESET", hex16(s.resetVector))}
          {renderRow("NMI", hex16(s.nmiVector))}
          {renderRow("IRQ", hex16(s.irqVector))}
        </dl>
        {switch Nullable.toOption(s.cart) {
        | Some(c) =>
          <>
            <h3 style={{marginTop: "1rem"}}> {React.string("Cartridge")} </h3>
            {cartSection(c)}
          </>
        | None =>
          <p style={{color: "#666", marginTop: "0.5rem"}}>
            {React.string("(no cartridge inserted)")}
          </p>
        }}
      </>

    let buttonStyle: JsxDOMStyle.t = {
      padding: "0.4rem 0.8rem",
      marginRight: "0.5rem",
      cursor: "pointer",
    }

    let canvasStyle: JsxDOMStyle.t = {
      width: "256px",
      height: "256px",
      imageRendering: "pixelated",
      border: "1px solid #aaa",
      background: "#222",
      display: "block",
    }

    /* sub palette 4 スロット */
    let subSwatches =
      <div style={{display: "flex", gap: "0.5rem"}}>
        {Belt.Array.makeBy(4, i => i)
        ->Belt.Array.map(i => {
          let mi = uint8ArrayLength(sub) >= 4 ? uint8At(sub, i) : 0
          let (r, g, b) =
            uint8ArrayLength(master) >= 192 ? readMaster(master, mi) : (0, 0, 0)
          <div
            key={Int.toString(i)}
            style={{textAlign: "center", fontSize: "0.75rem", color: "#444"}}>
            <Swatch
              r g b size=36 selected={i == activeSlot} title={hex8(mi)} onClick={onSubSlotClick(i)}
            />
            <div style={{marginTop: "0.25rem"}}>
              {React.string("slot " ++ Int.toString(i))}
            </div>
            <div style={{fontFamily: "monospace"}}> {React.string(hex8(mi))} </div>
          </div>
        })
        ->React.array}
      </div>

    /* master palette 4x16 grid */
    let masterGrid =
      <div
        style={{
          display: "grid",
          gridTemplateColumns: "repeat(16, 24px)",
          gridTemplateRows: "repeat(4, 24px)",
          gap: "2px",
        }}>
        {Belt.Array.makeBy(64, i => i)
        ->Belt.Array.map(i => {
          let (r, g, b) =
            uint8ArrayLength(master) >= 192 ? readMaster(master, i) : (0, 0, 0)
          <Swatch
            key={Int.toString(i)}
            r
            g
            b
            size=24
            selected={i == activeMasterIdx}
            title={hex8(i) ++ " " ++ cssHex(r, g, b)}
            label=?{showCode ? Some(hex8(i)) : None}
            onClick={onMasterClick(i)}
          />
        })
        ->React.array}
      </div>

    <div
      style={{
        fontFamily: "system-ui, sans-serif",
        maxWidth: "640px",
        margin: "2rem auto",
        padding: "1rem",
      }}>
      <h1> {React.string("FamiCaml")} </h1>
      <p style={{color: "#666"}}>
        {React.string("対応マッパー: NROM (0) / UNROM (2) / CNROM (3)")}
      </p>
      <section style={{marginTop: "1rem"}}>
        <input type_="file" accept=".nes" onChange={onRomChange} />
        {selectedName == ""
          ? React.null
          : <p style={{marginTop: "0.5rem", color: "#444"}}>
              {React.string("File: " ++ selectedName)}
            </p>}
      </section>
      <section style={{marginTop: "1rem"}}>
        <button onClick=onPowerOn style=buttonStyle> {React.string("Power ON")} </button>
        <button onClick=onPowerOff style=buttonStyle> {React.string("Power OFF")} </button>
        <button onClick=onReset style=buttonStyle> {React.string("Reset")} </button>
        <button onClick=onEject style=buttonStyle> {React.string("Eject")} </button>
      </section>
      {switch lastError {
      | Some(msg) =>
        <p style={{color: "#c00", marginTop: "1rem"}}>
          {React.string("Error: " ++ msg)}
        </p>
      | None => React.null
      }}
      <section style={{marginTop: "1rem"}}>
        <h2 style={{marginBottom: "0.5rem"}}> {React.string("Pattern Tables")} </h2>
        <div style={{display: "flex", gap: "1rem", flexWrap: "wrap"}}>
          <div>
            <p style={{margin: "0 0 0.25rem 0", fontSize: "0.85rem", color: "#444"}}>
              {React.string("Left ($0000-$0FFF)")}
            </p>
            <canvas
              ref={ReactDOM.Ref.domRef(canvasLeft)} width="128" height="128" style=canvasStyle
            />
          </div>
          <div>
            <p style={{margin: "0 0 0.25rem 0", fontSize: "0.85rem", color: "#444"}}>
              {React.string("Right ($1000-$1FFF)")}
            </p>
            <canvas
              ref={ReactDOM.Ref.domRef(canvasRight)} width="128" height="128" style=canvasStyle
            />
          </div>
        </div>
      </section>
      <section style={{marginTop: "1.5rem"}}>
        <h2 style={{marginBottom: "0.5rem"}}> {React.string("Palette")} </h2>
        <p style={{color: "#666", margin: "0 0 0.75rem 0", fontSize: "0.85rem"}}>
          {React.string(
            "Sub slot を選んで master grid から色をクリックで割り当て。Color picker でその master 色そのものを編集。",
          )}
        </p>
        <h3 style={{margin: "0.5rem 0 0.25rem 0", fontSize: "0.95rem"}}>
          {React.string("Sub Palette (4 slots)")}
        </h3>
        subSwatches
        <div
          style={{
            display: "flex",
            justifyContent: "space-between",
            alignItems: "baseline",
            margin: "0.75rem 0 0.25rem 0",
          }}>
          <h3 style={{margin: "0", fontSize: "0.95rem"}}>
            {React.string("Master Palette (64 colors)")}
          </h3>
          <label
            style={{
              fontSize: "0.8rem",
              color: "#444",
              cursor: "pointer",
              userSelect: "none",
            }}>
            <input
              type_="checkbox"
              checked={showCode}
              onChange={event =>
                setShowCode(_ => ReactEvent.Form.target(event)["checked"])}
              style={{marginRight: "0.25rem", verticalAlign: "middle"}}
            />
            {React.string("Show codes")}
          </label>
        </div>
        masterGrid
        <div
          style={{
            marginTop: "0.75rem",
            display: "flex",
            alignItems: "center",
            gap: "0.5rem",
            fontSize: "0.9rem",
          }}>
          <span>
            {React.string("Edit master " ++ hex8(activeMasterIdx) ++ ":")}
          </span>
          <input
            type_="color"
            value={cssHex(ar, ag, ab)}
            onChange={onColorPickerChange}
            style={{width: "48px", height: "32px", padding: "0", border: "1px solid #888"}}
          />
          <span style={{color: "#666", fontFamily: "monospace"}}>
            {React.string(cssHex(ar, ag, ab))}
          </span>
        </div>
        <div style={{marginTop: "0.75rem"}}>
          <button onClick=onExportPal style=buttonStyle> {React.string("Export .pal")} </button>
          <label style=buttonStyle>
            {React.string("Import .pal")}
            <input
              type_="file"
              accept=".pal"
              onChange={onPalChange}
              style={{display: "none"}}
            />
          </label>
          <button onClick=onResetPal style=buttonStyle> {React.string("Reset palette")} </button>
        </div>
      </section>
      {switch state {
      | Some(s) =>
        <section style={{marginTop: "1.5rem"}}>
          <h2 style={{marginBottom: "0.25rem"}}> {React.string("NES State")} </h2>
          {stateSection(s)}
        </section>
      | None => React.null
      }}
    </div>
  }
}

switch ReactDOM.querySelector("#root") {
| Some(root) =>
  let r = ReactDOM.Client.createRoot(root)
  ReactDOM.Client.Root.render(r, <App />)
| None => Console.error("#root not found")
}
