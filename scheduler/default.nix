{ mkDerivation, base, bytestring, conduit, containers, lib, process
, sydtest, timeout, unliftio
}:
mkDerivation {
  pname = "scheduler";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring conduit containers unliftio
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring process sydtest timeout unliftio
  ];
  homepage = "https://github.com/NorfairKing/scheduler#readme";
  license = "unknown";
  mainProgram = "scheduler-exe";
}
