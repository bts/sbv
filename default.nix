{ mkDerivation, array, async, base, bytestring, containers
, crackNum, deepseq, directory, doctest, filepath, generic-deriving
, ghc, Glob, hlint, mtl, pretty, process, QuickCheck, random
, reinterpret-cast, stdenv, syb, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, template-haskell, time, z3
}:
mkDerivation {
  pname = "sbv";
  version = "7.12.5";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array async base containers crackNum deepseq directory filepath
    generic-deriving ghc mtl pretty process QuickCheck random
    reinterpret-cast syb template-haskell time
  ];
  testHaskellDepends = [
    base bytestring containers directory doctest filepath Glob hlint
    mtl QuickCheck random reinterpret-cast syb tasty tasty-golden
    tasty-hunit tasty-quickcheck template-haskell
  ];
  testSystemDepends = [ z3 ];
  homepage = "http://leventerkok.github.com/sbv/";
  description = "SMT Based Verification: Symbolic Haskell theorem prover using SMT solving";
  license = stdenv.lib.licenses.bsd3;
}
