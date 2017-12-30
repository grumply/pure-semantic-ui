{ mkDerivation, base, ef, ef-base, pure, stdenv, tlc }:
mkDerivation {
  pname = "semantic-ui-pure";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ef ef-base pure tlc ];
  license = stdenv.lib.licenses.bsd3;
}
