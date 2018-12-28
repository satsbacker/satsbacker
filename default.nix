{ mkDerivation, aeson, array, attoparsec, base, base64-bytestring
, blaze-builder, bytestring, directory, entropy, http-client, lucid
, monad-logger, network, pwstore-fast, scotty, sqlite-simple
, stache, stdenv, text, unix, unordered-containers, vector, wai
, wai-extra, wai-middleware-static
}:
mkDerivation {
  pname = "satsbacker";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson array attoparsec base base64-bytestring blaze-builder
    bytestring directory entropy http-client lucid monad-logger network
    pwstore-fast scotty sqlite-simple stache text unix
    unordered-containers vector wai wai-extra wai-middleware-static
  ];
  homepage = "https://satsbacker.com";
  description = "An uncensorable membership platform for creators and backers";
  license = stdenv.lib.licenses.mit;
}
