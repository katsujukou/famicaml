/* ---- 外部 JS バインディング ---- */

type uint8Array
type arrayBuffer
@new external newUint8Array: arrayBuffer => uint8Array = "Uint8Array"

type file
@send external fileArrayBuffer: file => promise<arrayBuffer> = "arrayBuffer"
@get external fileName: file => string = "name"

type fileList
@get external fileListLength: fileList => int = "length"
@get_index external fileListItem: (fileList, int) => file = ""

@get external targetFiles: {..} => Nullable.t<fileList> = "files"

/* ---- wasm/main.ml が `Js.export "FamiCaml"` で公開する API ---- */

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

type famiCamlApi = {
  loadRom: uint8Array => loadResult,
  eject: unit => unit,
  reset: unit => unit,
  powerOn: unit => unit,
  powerOff: unit => unit,
  state: unit => nesState,
}

@val @scope("globalThis") external famiCaml: Nullable.t<famiCamlApi> = "FamiCaml"

/* ---- ヘルパー ---- */

let kbStr = bytes => Int.toString(bytes / 1024) ++ " KB"

let hex16 = n => {
  let s = Int.toString(n, ~radix=16)->String.toUpperCase
  let pad = Math.Int.max(0, 4 - String.length(s))
  "$" ++ String.repeat("0", pad) ++ s
}

/* ---- コンポーネント ---- */

module App = {
  @react.component
  let make = () => {
    let (state, setState) = React.useState((): option<nesState> => None)
    let (lastError, setLastError) = React.useState((): option<string> => None)
    let (selectedName, setSelectedName) = React.useState(() => "")

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

    let onChange = event => {
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

    let onEject    = _ => withApi(api => api.eject())
    let onReset    = _ => withApi(api => api.reset())
    let onPowerOn  = _ => withApi(api => api.powerOn())
    let onPowerOff = _ => withApi(api => api.powerOff())

    let renderRow = (label, value) =>
      <>
        <dt> {React.string(label)} </dt>
        <dd> {React.string(value)} </dd>
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
        <input type_="file" accept=".nes" onChange />
        {selectedName == ""
          ? React.null
          : <p style={{marginTop: "0.5rem", color: "#444"}}>
              {React.string("File: " ++ selectedName)}
            </p>}
      </section>

      <section style={{marginTop: "1rem"}}>
        <button onClick=onPowerOn  style=buttonStyle> {React.string("Power ON")}  </button>
        <button onClick=onPowerOff style=buttonStyle> {React.string("Power OFF")} </button>
        <button onClick=onReset    style=buttonStyle> {React.string("Reset")}     </button>
        <button onClick=onEject    style=buttonStyle> {React.string("Eject")}     </button>
      </section>

      {switch lastError {
      | Some(msg) =>
        <p style={{color: "#c00", marginTop: "1rem"}}>
          {React.string("Error: " ++ msg)}
        </p>
      | None => React.null
      }}

      {switch state {
      | Some(s) =>
        <section style={{marginTop: "1rem"}}>
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
