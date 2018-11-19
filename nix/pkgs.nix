{ args ? { config = import ./config.nix; }
, nixpkgs ? import <nixpkgs>
}:
let
  pkgs = nixpkgs args;
  overrideWith = override: default:
   let
     try = builtins.tryEval (builtins.findFile builtins.nixPath override);
   in if try.success then
     builtins.trace "using search host <${override}>" try.value
   else
     default;
in
let
  # save the nixpkgs value in pkgs'
  # so we can work with `pkgs` provided by modules.
  pkgs' = pkgs;
  # all packages from hackage as nix expressions
  hackage = import (overrideWith "hackage"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "hackage.nix";
                                            rev    = "d8e03ec0e3c99903d970406ae5bceac7d993035d";
                                            sha256 = "0c7camspw7v5bg23hcas0r10c40fnwwwmz0adsjpsxgdjxayws3v";
                                            name   = "hackage-exprs-source"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "haskell.nix";
                                            rev    = "2a3b2612a15fd7f14d32c3519aba2b64bd7b1e43";
                                            sha256 = "181dv1zlf381kkb82snjmpibhgmkyw1n5qsvpqjrv8dxmcjqjl2k";
                                            name   = "haskell-lib-source"; }))
                   hackage;

  # the set of all stackage snapshots
  stackage = import (overrideWith "stackage"
                     (pkgs.fetchFromGitHub { owner  = "angerman";
                                             repo   = "stackage.nix";
                                             rev    = "67675ea78ae5c321ed0b8327040addecc743a96c";
                                             sha256 = "1ds2xfsnkm2byg8js6c9032nvfwmbx7lgcsndjgkhgq56bmw5wap";
                                             name   = "stackage-snapshot-source"; }))
                   ;

  # our packages
  stack-pkgs = import ./.stack-pkgs.nix;

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  pkgSet = haskell.mkNewPkgSet {
    inherit pkgs;
    pkg-def = stackage.${stack-pkgs.resolver};
    pkg-def-overlays = [
      stack-pkgs.overlay
      # We use some customized libiserv/remote-iserv/iserv-proxy
      # instead of the ones provided by ghc. This is mostly due
      # to being able to hack on them freely as needed.
      #
      # iserv is only relevant for template-haskell execution in
      # a cross compiling setup.
      {
        ghci         = ./ghci.nix;
        ghc-boot     = ./ghc-boot.nix;
        libiserv     = ./libiserv.nix;
        remote-iserv = ./remote-iserv.nix;
        iserv-proxy  = ./iserv-proxy.nix;
#        packages.hfsevents.revision = import ../hfsevents-0.1.6;
      }
      (hackage: {
          hsc2hs = hackage.hsc2hs."0.68.4".revisions.default;
          # stackage 12.17 beautifully omitts the Win32 pkg
          Win32 = hackage.Win32."2.6.2.0".revisions.default;
      })
    ];
    modules = [
      {
         # This needs true, otherwise we miss most of the interesting
         # modules.
         packages.ghci.flags.ghci = true;
         # this needs to be true to expose module
         #  Message.Remote
         # as needed by libiserv.
         packages.libiserv.flags.network = true;
      }
      ({ config, ... }: {
          packages.hsc2hs.components.exes.hsc2hs.doExactConfig= true;
          packages.Win32.components.library.build-tools = [ config.hsPkgs.buildPackages.hsc2hs ];
#          packages.Win32.components.library.doExactConfig = true;
          packages.remote-iserv.postInstall = ''
            cp ${pkgs.windows.mingw_w64_pthreads}/bin/libwinpthread-1.dll $out/bin/
          '';
      })
      {
        packages.conduit.patches            = [ ./patches/conduit-1.3.0.2.patch ];
        packages.cryptonite-openssl.patches = [ ./patches/cryptonite-openssl-0.7.patch ];
        packages.streaming-commons.patches  = [ ./patches/streaming-commons-0.2.0.0.patch ];
        packages.x509-system.patches        = [ ./patches/x509-system-1.6.6.patch ];

        packages.file-embed-lzma.patches    = [ ./patches/file-embed-lzma-0.patch ];
      }
      ({ lib, ... }: {
        # packages.cardano-sl-infra.configureFlags = lib.mkForce [ "--ghc-option=-v3" ];
        # packages.cardano-sl-infra.components.library.configureFlags = lib.mkForce [ "--ghc-option=-v3" ];
#        packages.cardano-sl-infra.components.library.configureFlags = [ "-v" "--ghc-option=-v3" ];
#        packages.cardano-sl-infra.components.library.setupBuildFlags = [ "-v" ];
      })
      # cross compilation logic
      ({ pkgs, buildModules, config, lib, ... }:
      let
        buildFlags = map (opt: "--ghc-option=" + opt) [
          "-fexternal-interpreter"
          "-pgmi" "${config.hsPkgs.buildPackages.iserv-proxy.components.exes.iserv-proxy}/bin/iserv-proxy"
          "-opti" "127.0.0.1" "-opti" "$PORT"
          # TODO: this should be automatically injected based on the extraLibrary.
          "-L${pkgs.windows.mingw_w64_pthreads}/lib"
          "-L${pkgs.gmp}/lib"
        ];
        preBuild = ''
          # unset the configureFlags.
          # configure should have run already
          # without restting it, wine might fail
          # due to a too large environment.
          unset configureFlags
          PORT=$((5000 + $RANDOM % 5000))
          echo "---> Starting remote-iserv on port $PORT"
          WINEDLLOVERRIDES="winemac.drv=d" WINEDEBUG=-all+error WINEPREFIX=$TMP ${pkgs.buildPackages.winePackages.minimal}/bin/wine64 ${packages.remote-iserv.components.exes.remote-iserv}/bin/remote-iserv.exe tmp $PORT &
          echo "---| remote-iserv should have started on $PORT"
          RISERV_PID=$!
        '';
        postBuild = ''
          echo "---> killing remote-iserv..."
          kill $RISERV_PID
        '';

        testFlags = [ "--test-wrapper ${wineTestWrapper}/bin/test-wrapper" ];
        wineTestWrapper = pkgs'.writeScriptBin "test-wrapper" ''
          #!${pkgs'.stdenv.shell}
          set -euo pipefail
          WINEDLLOVERRIDES="winemac.drv=d" WINEDEBUG=-all+error LC_ALL=en_US.UTF-8 WINEPREFIX=$TMP ${pkgs.buildPackages.winePackages.minimal}/bin/wine64 $@*
        '';
        preCheck = ''
          echo "================================================================================"
          echo "RUNNING TESTS for $name via wine64"
          echo "================================================================================"
          # copy all .dlls into the local directory.
          # we ask ghc-pkg for *all* dynamic-library-dirs and then iterate over the unique set
          # to copy over dlls as needed.
          for libdir in $(ghc-pkg --package-db=$packageConfDir field "*" dynamic-library-dirs --simple-output|xargs|sed 's/ /\n/g'|sort -u); do
            if [ -d "$libdir" ]; then
              for lib in "$libdir"/*.{DLL,dll}; do
                cp "$lib" .
              done
            fi
          done
        '';
        postCheck = ''
          echo "================================================================================"
          echo "END RUNNING TESTS"
          echo "================================================================================"
        '';
        withTH = { setupBuildFlags = buildFlags; setupTestFlags = testFlags;
                   inherit preBuild postBuild preCheck postCheck; };
        in lib.optionalAttrs pkgs'.stdenv.hostPlatform.isWindows  {
         packages.generics-sop      = withTH;
         packages.ether             = withTH;
         packages.th-lift-instances = withTH;
         packages.aeson             = withTH;
         packages.hedgehog          = withTH;
         packages.th-orphans        = withTH;
         packages.uri-bytestring    = withTH;
         packages.these             = withTH;
         packages.katip             = withTH;
         packages.swagger2          = withTH;
         packages.wreq              = withTH;
         packages.wai-app-static    = withTH;
         packages.log-warper        = withTH;
         packages.cardano-sl-util   = withTH;
         packages.cardano-sl-crypto = withTH;
         packages.cardano-sl-crypto-test = withTH;
         packages.cardano-sl-core   = withTH;
         packages.cardano-sl        = withTH;
         packages.cardano-sl-chain  = withTH;
         packages.cardano-sl-db     = withTH;
         packages.cardano-sl-networking = withTH;
         packages.cardano-sl-infra  = withTH;
         packages.cardano-sl-infra-test = withTH;
         packages.cardano-sl-client = withTH;
         packages.cardano-sl-core-test = withTH;
         packages.cardano-sl-chain-test = withTH;
         packages.cardano-sl-utxo   = withTH;
         packages.cardano-sl-wallet-new = withTH;
         packages.cardano-sl-tools    = withTH;
         packages.cardano-sl-generator = withTH;
         packages.cardano-sl-auxx     = withTH;
         packages.cardano-sl-faucet   = withTH;
         packages.cardano-sl-binary   = withTH;
         packages.cardano-sl-node     = withTH;
         packages.cardano-sl-explorer = withTH;
         packages.math-functions    = withTH;
         packages.servant-swagger-ui = withTH;
         packages.servant-swagger-ui-redoc = withTH;
         packages.trifecta            = withTH;
         packages.Chart               = withTH;
         packages.active              = withTH;
         packages.diagrams            = withTH;
         packages.diagrams-lib        = withTH;
         packages.diagrams-svg        = withTH;
         packages.diagrams-postscript = withTH;
         packages.Chart-diagrams      = withTH;
      })
      # packages we wish to ignore version bounds of.
      # this is similar to jailbreakCabal, however it
      # does not require any messing with cabal files.
      {
         packages.katip.components.library.doExactConfig         = true;
         packages.serokell-util.components.library.doExactConfig = true;
      }
      ({ pkgs, ... }: {
         # ???: Why do I ned CoreServices to be part of the `libs`? It's a
         #      Framework after all.  This is quite confusing to me.
         packages.hfsevents.components.library.frameworks  = [ pkgs.CoreServices ];
#         packages.hfsevents.components.library.build-tools = [ pkgs.CoreServices ];
#         packages.hfsevents.components.library.configureFlags = [ "-v" "--ghc-option=-v3" ];
#         packages.hfsevents.components.library.setupBuildFlags = [ "-v" ];
      })
    ];
  };

  packages = pkgSet.config.hsPkgs // { _config = pkgSet.config; };

in packages
