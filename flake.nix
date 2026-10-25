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

      # projects known to fail with the reason why (CI checks that they still fail)
      broken = {
        # foobar = "fails since 0.99.0: 'Cannot typecheck call' in test/foobar.effekt";
      };

      version = "0.999.999+nightly";
      depsSha256 = "sha256-YVGYTc76xAYJW2tj6o3TRBoAEhw7I2KNaFJRrQo8XJw=";

      perSystem = system:
        let
          effektLib = mkLib nixpkgs.legacyPackages.${system};
        in
        {
          inherit effektLib;
          effekt = effektLib.buildEffektFromSource {
            inherit version depsSha256;
            src = self;
            backends = _: effektLib.availableBackends version;
          };
        };

      allSystems = forAllSystems perSystem;

      # what is expected of a project with the compiler under test
      expectationOf = name: if broken ? ${name} then "broken" else "community";

      # returns the package for the given project with an overriden Effekt,
      # checking that the project has a package to begin with
      packageUnderTest = system: name: project:
        if project ? packages
        then project.packages.${system}.default.override { inherit (allSystems.${system}) effekt; }
        else throw "community project '${name}' publishes no packages";

      # every backend of a project as a job ('<pname>-<backend>', see 'flakePackages'), none if the project offers
      # no package for the system or its package is not meant for it ('meta.platforms')
      jobsFor = system: name:
        let
          project = projects.${name};
          package = packageUnderTest system name project;
          offered = !(project ? packages) || project.packages ? ${system};
        in
        lib.optionalAttrs (offered && lib.meta.availableOn (lib.systems.elaborate system) package)
          (allSystems.${system}.effektLib.flakePackages package);

      # jobs grouped by expectation (both groups always exist, so that CI finds 'broken' even when it is empty)
      jobs = forAllSystems (system:
        lib.mapAttrs (_: names: lib.genAttrs names (jobsFor system))
          ({ community = [ ]; broken = [ ]; } // builtins.groupBy expectationOf (builtins.attrNames projects)));

      unknownBroken = builtins.filter (name: !(projects ? ${name})) (builtins.attrNames broken);
    in
    assert lib.assertMsg (unknownBroken == [ ]) "'broken' names unknown community projects: ${toString unknownBroken}";
    {
      # the compiler under test
      packages = forAllSystems (system: {
        inherit (allSystems.${system}) effekt;
        default = allSystems.${system}.effekt;
      });

      # the jobs, as '<expectation>.<project>.<pname>-<backend>' ('community' expected to build, 'broken' to fail)
      legacyPackages = jobs;

      lib = {
        inherit broken;

        # the projects not built for each system (broken, or without a package meant for it)
        skipped = forAllSystems (system:
          builtins.filter (name: (jobs.${system}.community.${name} or { }) == { }) (builtins.attrNames projects));
      };

      devShells = forAllSystems (system:
        let
          inherit (allSystems.${system}) effektLib effekt;
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
