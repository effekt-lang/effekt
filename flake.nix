{
  description = "Local Effekt build using the Effekt Nix flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    effekt-nix.url = "github:jiribenes/effekt-nix";
    sbt-derivation = {
      url = "github:zaninime/sbt-derivation";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, effekt-nix, sbt-derivation }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        effekt-lib = effekt-nix.lib.${system};

        buildEffektFromSource = {
          src,
          version,
          depsSha256, # SHA256 of the Scala dependencies
          backends ? [effekt-lib.effektBackends.js],
        }:
          assert backends != []; # Ensure at least one backend is specified
          sbt-derivation.lib.mkSbtDerivation {
            inherit pkgs;
            pname = "effekt";
            inherit version;
            inherit src;

            nativeBuildInputs = [pkgs.nodejs pkgs.maven pkgs.makeWrapper pkgs.gnused];
            buildInputs = [pkgs.jre] ++ pkgs.lib.concatMap (b: b.buildInputs) backends;

            inherit depsSha256;
            depsArchivalStrategy = "copy";

            # Change the version in build.sbt
            prePatch = ''
              sed -i 's/lazy val effektVersion = "[^"]*"/lazy val effektVersion = "${version}"/' build.sbt
            '';

            buildPhase = ''
              export MAVEN_OPTS="-Dmaven.repo.local=$out/.m2/repository"
              sbt assembleBinary
            '';

            installPhase = ''
              mkdir -p $out/bin $out/lib
              mv bin/effekt $out/lib/effekt.jar
              mv libraries $out/libraries

              makeWrapper ${pkgs.jre}/bin/java $out/bin/effekt \
                --add-flags "-jar $out/lib/effekt.jar" \
                --prefix PATH : ${pkgs.lib.makeBinPath (pkgs.lib.concatMap (b: b.buildInputs) backends)}
            '';
          };

        # Use builtins.path to include all files, including submodules
        src = builtins.path {
          path = "/Users/jb/dev/eff/effekt";
          name = "effekt-src";
          filter = path: type: 
            baseNameOf path != ".git" &&
            baseNameOf path != "result" &&
            baseNameOf path != "nix-build-result";
        };

        # Build Effekt from the local source
        localEffekt = buildEffektFromSource {
          inherit src;
          version = "0.99.99+nightly";
          depsSha256 = "sha256-uMtkd3lfkXM8gjmFoEw8NuE4dJtuRwb1XN6mUyLDRaM=";
          backends = builtins.attrValues effekt-lib.effektBackends;
        };

        compilerDevShell = effekt-nix.devShells.${system}.compilerDev;
      in {
        # App for the local Effekt build
        apps.default = flake-utils.lib.mkApp {
          drv = localEffekt;
          name = "effekt";
        };

        devShells = {
          # Dev shell for the local Effekt build
          default = effekt-lib.mkDevShell {
            effekt = localEffekt;
            backends = with effekt-lib.effektBackends; [js llvm];
          };
          # Compiler development shell
          compilerDev = compilerDevShell;
        };

        # Package for the local Effekt build
        packages.default = localEffekt;
      }
    );
}
