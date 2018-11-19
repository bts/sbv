let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          crackNum = haskellPackagesNew.callPackage ./crackNum.nix { };

          sbv = haskellPackagesNew.callPackage ./default.nix {
            z3 = pkgs.z3;
          };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { sbv = pkgs.haskellPackages.sbv;
  }
