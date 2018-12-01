{ mkDerivation, base, pure-cond, pure-lifted, pure, pure-txt, pure-uri, stdenv }:
mkDerivation {
  pname = "pure-semantic-ui";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-cond pure-lifted pure pure-txt pure-uri ];
  license = stdenv.lib.licenses.bsd3;
}
