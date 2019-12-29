{ mkDerivation, aeson, attoparsec, base, base-compat, blaze-html
, blaze-markup, bytestring, directory, http-media, lucid, mtl
, servant, servant-server, stdenv, string-conversions, text, time
, wai, warp
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base base-compat blaze-html blaze-markup
    bytestring directory http-media lucid mtl servant servant-server
    string-conversions text time wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
