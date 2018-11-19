{ mkDerivation, base, FloatingHex, array , stdenv, fetchFromGitHub
}:
mkDerivation {
  pname = "crackNum";
  version = "2.3";
  src = fetchFromGitHub {
    owner  = "LeventErkok";
    repo   = "crackNum";
    rev    = "54cf70861a921062db762b3c50e933e73446c3b2";
    sha256 = "02cg64rq8xk7x53ziidljyv3gsshdpgbzy7h03r869gj02l7bxwa";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base FloatingHex array
  ];
  executableHaskellDepends = [
    base FloatingHex array
  ];
  homepage = "http://github.com/LeventErkok/CrackNum";
  description = "Crack various integer, floating-point data formats";
  license = stdenv.lib.licenses.bsd3;
}
