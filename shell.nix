with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, base, bytestring, hspec, HUnit, lens
             , lens-family-core, lens-family-th, QuickCheck, stdenv, text, wreq
             }:
             mkDerivation {
               pname = "stellar-haskell";
               version = "0.1.0.0";
               sha256 = "0";
               buildDepends = [
                 aeson base bytestring HUnit lens lens-family-core lens-family-th
                 QuickCheck text wreq
               ];
               testDepends = [ aeson base bytestring hspec lens QuickCheck text ];
               description = "Stellar API for Haskell";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
