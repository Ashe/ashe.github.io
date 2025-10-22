{
  description = "Website flake for aas.sh";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem ( system:
    let

      ########################
      ## Third party assets ##
      ########################

      # Download third party
      bulma = (builtins.fetchTarball {
        url = "https://github.com/jgthms/bulma/archive/refs/tags/0.9.4.tar.gz";
        sha256 = "0j12iv99xx3mvyqpqa8k3grhdyp8061b74k61g54cmdyfm3gqdmm";
      });
      line-awesome = (builtins.fetchTarball {
        url = "https://github.com/icons8/line-awesome/archive/refs/tags/v1.2.1.tar.gz";
        sha256 = "1m69lzlvjdpldpqqxhh2ylgjc0sz119yq4mlcz12c314kgpiv90f";
      });
      revealjs = (builtins.fetchTarball {
        url = "https://github.com/hakimel/reveal.js/archive/refs/tags/4.4.0.tar.gz";
        sha256 = "1pirbc65fvajnnakni1x038jfy7av59xaihpkbpgfn7s2il618vf";
      });
      vanillajs-scrollspy = (builtins.fetchTarball {
        url = "https://github.com/ederssouza/vanillajs-scrollspy/archive/refs/tags/3.0.3.tar.gz";
        sha256 = "0g4p7jqwp7isd22lwaxqygi5sal5cpwmcqmm48jbngzv99pqc0r3";
      });
      swiperjs = (builtins.fetchTarball {
        url = "https://registry.npmjs.org/swiper/-/swiper-8.3.2.tgz";
        sha256 = "1bn2zfg668zaj3sacqqnqn7df82801yq11wgx34xrd5qh6297x68";
      });
      katex = (builtins.fetchTarball {
        url = "https://github.com/KaTeX/KaTeX/releases/download/v0.16.4/katex.tar.gz";
        sha256 = "0pgajj8vqfvgws3wnn5bfy4h3w75n4dz8jvjsxcw8s80jnp0md8c";
      });

      # Gather third party libraries in one place
      thirdparty = [
        {
          name = "bulma";
          path = "${bulma}/sass";
        }
        {
          name = "line-awesome";
          path = "${line-awesome}/dist/line-awesome";
        }
        {
          name = "katex";
          path = "${katex}";
        }
        {
          name = "revealjs";
          path = "${revealjs}";
        }
        {
          name = "swiperjs";
          path = "${swiperjs}";
        }
        {
          name = "vanillajs-scrollspy";
          path = "${vanillajs-scrollspy}/dist";
        }
      ];

      ################
      ## Site setup ##
      ################

      pkgs = import nixpkgs { inherit system; };
      toApp = drv: { type = "app"; program = "${drv}";};

      # Create a symlink to third party directories
      thirdpartyDir = pkgs.linkFarm "thirdparty" thirdparty;

      # Read package file
      website = pkgs.callPackage ./site.nix {
        inherit pkgs thirdparty;
      };

    in {

      # Build the website as a package
      packages = flake-utils.lib.flattenTree {
        default = website.site-with-thirdparty;
      };

      # Allows for commands like 'nix run .#watch'
      apps = rec {
        rebuild = toApp website.ci.rebuild;
        watch = toApp website.ci.watch;
        rebuild-watch = toApp website.ci.rebuild-watch;
        clean = toApp website.ci.clean;
        site = toApp website.site-with-thirdparty;
        default = rebuild-watch;
      };

      # Enable haskell development
      devShells.default = pkgs.haskellPackages.shellFor {

        # Specify which packages to include in the environment
        packages = p: [ website.site ];

        # Build tools and development utilities
        buildInputs = with pkgs; [
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.ghcid
          haskellPackages.ormolu
          haskellPackages.hlint
          git
        ];

        # Environment variables for your application
        shellHook = ''
          export THIRDPARTY="${thirdpartyDir}"
          echo "✓ Haskell development shell ready"
          echo "✓ Thirdparty assets available at: \$THIRDPARTY"
          echo "✓ Use: cabal run, cabal build, cabal repl, etc."
        '';

        # Make the thirdparty directory available to Nix builds too
        THIRDPARTY = thirdpartyDir;
      };
    }
  );
}
