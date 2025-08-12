{ config, nix-haskell-patches, ... }:

let nix-thunk = config.importing.nix-thunk;
    deps = with nix-thunk; mapSubdirectories thunkSource ./deps;

in {
  imports = [
    "${nix-haskell-patches}/js/splitmix"
  ];

  name = "reflex-dom-attrs";
  src = ./.;
  compiler-nix-name = "ghc912";

  shell = {
    crossPlatforms = ps: with ps; [ ghcjs ];
    packages = ps: with ps; [ reflex-dom-attrs ];
    withHaddock = false;
    withHoogle = false;
  };
}
