{
  description = "A Nix-flake-based Rust development environment";

  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.*.tar.gz";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    rust-overlay,
  }: let
    overlays = [
      rust-overlay.overlays.default
      (final: prev: {
        rustToolchain = let
          rust = prev.rust-bin;
        in
          if builtins.pathExists ./rust-toolchain.toml
          then rust.fromRustupToolchainFile ./rust-toolchain.toml
          else if builtins.pathExists ./rust-toolchain
          then rust.fromRustupToolchainFile ./rust-toolchain
          else
            rust.stable.latest.default.override {
              extensions = ["rust-src" "rustfmt"];
            };
      })
    ];
    supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forEachSupportedSystem = f:
      nixpkgs.lib.genAttrs supportedSystems (system:
        f {
          pkgs = import nixpkgs {inherit overlays system;};
        });
  in {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
    devShells = forEachSupportedSystem ({pkgs}: {
      default = pkgs.mkShell {
        # for nannou
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
          pkgs.vulkan-loader
        ];
        packages = with pkgs; [
          rustToolchain
          openssl
          pkg-config
          cargo-deny
          cargo-edit
          cargo-watch
          rust-analyzer
          gdb
          # for nannou
          xorg.libX11
          xorg.libXcursor
          xorg.libXrandr
          xorg.libXi
          wayland
          vulkan-loader
          udev
          cmake
          gcc
          nodejs
          wasm-pack
          # for webdev
          typescript
          nodePackages.typescript-language-server
          nodePackages.prettier
          vscode-langservers-extracted
          tailwindcss-language-server
          nodePackages.svelte-check
          nodePackages.svelte-language-server
        ];
      };
    });
    packages = forEachSupportedSystem ({pkgs}: rec {
      vesmor_pkg = pkgs.rustPlatform.buildRustPackage {
        pname = "vesmor_pkg";
        version = "0.1.0";

        src = ./.;

        cargoHash = "sha256-wr/SaBeGZRFaoqdJdI+tYiNIyrmKpfgURyl7uvqnMtA=";

        CARGO_BUILD_TARGET = "wasm32-unknown-unknown";

        vesmish = ./vesmish;

        # hack to have local vesmish source
        postUnpack = ''
          mv *-source/vesmor/* *-source/
        '';
        cargoPatches = [
          ./Cargo.toml.patch
        ];

        nativeBuildInputs = with pkgs; [
          rustToolchain
          wasm-bindgen-cli
          wasm-pack
          binaryen
        ];

        buildPhase = ''
          export WASM_PACK_CACHE=/build/.wasm-pack-cache
          wasm-pack build --out-dir $out/pkg
        '';

        installPhase = ''echo Skipping Install Phase'';

        checkPhase = ''echo Skipping Check Phase'';

        meta = with pkgs.lib; {
          description = "";
          homepage = "";
          # license = licenses.unlicense;
          maintainers = [];
        };
      };

      vesmor_web = pkgs.buildNpmPackage {
        pname = "vesmor_web";
        version = "0.1";

        src = ./vesmor;

        vesmish = ./vesmish;

        vesmor_pkg = vesmor_pkg;

        nativeBuildInputs = with pkgs; [
          nodejs
          nodePackages.webpack
          nodePackages.webpack-cli
        ];
        buildInputs = [
        ];

        buildPhase = ''
        echo $vesmor_pkg
        ls -lah $vesmor_pkg
        cp $vesmor_pkg/pkg ./pkg -r
        ls -lah
        ls -lah ./pkg
        webpack --mode production -o $out/web
        '';

        npmDepsHash = "sha256-9eTJrzE4HgALFXQZImj/IGFQ2eK5bccFoccPegvm1XI=";

        outputs = ["out"];

        meta = {
          description = "Web build of Vesmor console";
          homepage = "";
          # license = lib.licenses.;
          maintainers = with pkgs.lib.maintainers; [];
        };
      };
    });
  };
}
