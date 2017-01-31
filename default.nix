{ mkDerivation, attoparsec, base, bytestring, bytestring-lexing
, case-insensitive, hspec, hspec-core, http-types, iso8601-duration
, mime, QuickCheck, quickcheck-instances, scientific
, spatial-reference, stdenv, text, time
}:
mkDerivation {
  pname = "wai-ogc";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring bytestring-lexing case-insensitive
    http-types iso8601-duration mime scientific spatial-reference text
    time
  ];
  testHaskellDepends = [
    base bytestring hspec hspec-core http-types QuickCheck
    quickcheck-instances text time
  ];
  homepage = "https://github.com/albertov/wai-ogc";
  description = "Opengeospatial types and request parsing";
  license = stdenv.lib.licenses.bsd3;
}
