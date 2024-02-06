{ mkDerivation, base, lib }:
mkDerivation {
  pname = "scheduler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/scheduler#readme";
  license = lib.licenses.bsd3;
  mainProgram = "scheduler-exe";
}
