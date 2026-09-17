{
  description = "Local Effekt build including the community repos";

  inputs = {
    self.submodules = true; # kiama

    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    effekt-nix = {
      url = "github:jiribenes/effekt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # === COMMUNITY BUILD ===
    # Please keep this list sorted.
    AnsatzLite = { url = "github:effekt-community/AnsatzLite"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    effekt-compression = { url = "github:effekt-community/effekt-compression"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    effekt-rejit = { url = "github:effekt-community/effekt-rejit"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    effekt-stm = { url = "github:effekt-community/effekt-stm"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    Effekt8 = { url = "github:effekt-community/Effekt8"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    EffektChess = { url = "github:effekt-community/EffektChess"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    effektive-zombies = { url = "github:effekt-community/effektive-zombies"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    EffektPy = { url = "github:effekt-community/EffektPy"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    fair-minesweeper = { url = "github:effekt-community/fair-minesweeper"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    FairSweeper = { url = "github:effekt-community/FairSweeper"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    LinkedIn-Games-in-Effekt = { url = "github:effekt-community/LinkedIn-Games-in-Effekt"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    nette-effekte = { url = "github:effekt-community/nette-effekte"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    Real-Time-Stream-Processing-Library = { url = "github:effekt-community/Real-Time-Stream-Processing-Library"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    template = { url = "github:jiribenes/effekt-template"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    Toki-Pona-Translator = { url = "github:effekt-community/Toki-Pona-Translator"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    tracing-effective-rays = { url = "github:effekt-community/tracing-effective-rays"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    Turn-based-Strategy-Name = { url = "github:effekt-community/Turn-based-Strategy-Name"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    ultimate-tictactoe = { url = "github:effekt-community/ultimate-tictactoe"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
    wave-management-sim = { url = "github:effekt-community/wave-management-sim"; inputs.nixpkgs.follows = "nixpkgs"; inputs.effekt-nix.follows = "effekt-nix"; };
  };

  outputs = inputs@{ self, nixpkgs, effekt-nix, ... }:
    let
      inherit (effekt-nix.lib) mkLib supportedSystems;
      inherit (nixpkgs) lib;
      forAllSystems = lib.genAttrs supportedSystems;

      # everything that is not our infrastructure is a community project
      projects = builtins.removeAttrs inputs [ "self" "nixpkgs" "effekt-nix" ];

      version = "0.999.999+nightly";
      depsSha256 = "sha256-qcFOzFgf3m4OSWzlaq9w1LpWAHXV86Q4mNMQWZ4GFvs=";

      perSystem = forAllSystems (system:
        let
          effektLib = mkLib (import nixpkgs { inherit system; });
        in
        {
          inherit effektLib;
          effekt = effektLib.buildEffektFromSource {
            inherit version depsSha256;
            src = self;
            backends = _: effektLib.availableBackends version;
          };
        });

      # filter out projects that don't offer package for the given system
      projectsFor = system: lib.filterAttrs
        (_: project: !(project ? packages) || project.packages ? ${system}) projects;

      # return the package for the given project with an overriden Effekt, checking that the project has a package to begin with
      packageFor = system: effekt: name: project:
        if project ? packages
        then project.packages.${system}.default.override { inherit effekt; }
        else throw "community project '${name}' publishes no packages";
    in {
      # one package per community project ~> a parallel builder reports each one separately
      packages = forAllSystems (system:
        let
          inherit (perSystem.${system}) effekt;
        in
        lib.mapAttrs (packageFor system effekt) (projectsFor system)
        // {
          inherit effekt;
          default = effekt;
        });

      devShells = forAllSystems (system:
        let
          inherit (perSystem.${system}) effektLib effekt;
        in
        {
          default = effektLib.mkDevShell {
            inherit effekt;
            backends = bs: [ bs.js bs.llvm ];
          };
          compilerDev = effekt-nix.devShells.${system}.compilerDev;
        });
    };
}
