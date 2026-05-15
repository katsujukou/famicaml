.PHONY: wasm wasm-copy ui-install ui-dev ui-build clean

wasm:
	dune build wasm/

wasm-copy: wasm
	rm -rf ui/public/wasm
	mkdir -p ui/public/wasm
	cp _build/default/wasm/main.bc.wasm.js ui/public/wasm/
	cp -r _build/default/wasm/main.bc.wasm.assets ui/public/wasm/
	chmod -R u+w ui/public/wasm

ui-install:
	cd ui && pnpm install

ui-dev: wasm-copy
	cd ui && pnpm dev

ui-build: wasm-copy
	cd ui && pnpm run build

clean:
	dune clean
	rm -rf ui/public/wasm
	rm -rf ui/dist ui/lib