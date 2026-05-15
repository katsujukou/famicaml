{
  description = "Project FamiCaml";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Override the OCaml packages to set `dontStrip = true` for `wasm_of_ocaml-compiler`.
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3.overrideScope
          (final: prev: {
            wasm_of_ocaml-compiler = prev.wasm_of_ocaml-compiler.overrideAttrs (old: {
              dontStrip = true;
            });
          });

        ocamlDeps = with ocamlPackages; [
          cmdliner
          base
          stdint
          js_of_ocaml
          js_of_ocaml-ppx
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = ocamlDeps ++ (with ocamlPackages; [
            ocaml
            dune_3
            findlib
            js_of_ocaml-compiler
            wasm_of_ocaml-compiler
            ocaml-lsp
            ocamlformat
            utop
            odoc
          ]) ++ (with pkgs; [
            nodejs_22
            pnpm
            binaryen
          ]);

          shellHook = ''
            echo "OCaml:          $(ocaml -version)"
            echo "dune:           $(dune --version)"
            echo "wasm_of_ocaml:  $(wasm_of_ocaml --version 2>/dev/null || echo n/a)"
            echo "node:           $(node --version)"
          '';
        };

        packages.default = ocamlPackages.buildDunePackage {
          pname = "famicaml";
          version = "0.1.0";
          src = ./.;
          duneVersion = "3";
          buildInputs = ocamlDeps;
        };
      });
}