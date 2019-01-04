let _nixpkgs = import <nixpkgs> {}; in

with import (_nixpkgs.fetchFromGitHub {
    owner = "NixOS"
    ; repo = "nixpkgs"
    ; rev = "4c943d77e2e44bec7eca2688d63f54883fd8c8ab"
    ; sha256 = "0br0b3x3z600mlrapx1ylyr9m5a4693llv5a7bvlbsi3i57sr5sh";
    }) {};

ocamlPackages.buildDunePackage rec {
  pname = "game_of_life";
  version = "0";

  minimumOCamlVersion = "4.03";

  src = ./.;

  buildInputs = [ ];
  propagatedBuildInputs = with ocamlPackages;
    [batteries lambdaTerm lwt3 camomile];
}
