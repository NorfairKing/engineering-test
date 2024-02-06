{ mkDerivation, base, bytestring, conduit, lib, process, sydtest
, timeout, unliftio
}:
mkDerivation {
  pname = "scheduler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring conduit unliftio ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring process sydtest timeout unliftio
  ];
  homepage = "https://github.com/NorfairKing/scheduler#readme";
  license = lib.licenses.bsd3;
  mainProgram = "scheduler-exe";
}
