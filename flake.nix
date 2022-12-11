{
  description = "Website flake for aas.sh";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    bulma = {
      url = "git+https://github.com/jgthms/bulma/";
      flake = false;
    };
    line-awesome = {
      url = "git+https://github.com/icons8/line-awesome/";
      flake = false;
    };
    revealjs = {
      url = "git+https://github.com/hakimel/reveal.js/";
      flake = false;
    };
    vanillajs-scrollspy = {
      url = "git+https://github.com/ederssouza/vanillajs-scrollspy/";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem ( system:
    let
      pkgs = import nixpkgs { inherit system; };
      toApp = drv: { type = "app"; program = "${drv}";};
      swiperjs = (builtins.fetchTarball {
        url = "https://registry.npmjs.org/swiper/-/swiper-8.3.2.tgz";
        sha256 = "1bn2zfg668zaj3sacqqnqn7df82801yq11wgx34xrd5qh6297x68";
      });
      katex = (builtins.fetchTarball {
        url = "https://github.com/KaTeX/KaTeX/releases/download/v0.16.4/katex.tar.gz";
        sha256 = "0pgajj8vqfvgws3wnn5bfy4h3w75n4dz8jvjsxcw8s80jnp0md8c";
      });
      website = pkgs.callPackage ./site.nix {
        inherit pkgs;
        thirdparty = [
          {
            name = "bulma";
            path = "${inputs.bulma}/sass";
          }
          {
            name = "line-awesome";
            path = "${inputs.line-awesome}/dist/line-awesome";
          }
          {
            name = "katex";
            path = "${katex}";
          }
          {
            name = "revealjs";
            path = "${inputs.revealjs}";
          }
          {
            name = "swiperjs";
            path = "${swiperjs}";
          }
          {
            name = "vanillajs-scrollspy";
            path = "${inputs.vanillajs-scrollspy}/dist";
          }
        ];
      };
    in {
      packages = flake-utils.lib.flattenTree {
        inherit (website);
        default = website.site-with-thirdparty;
      };
      apps = rec {
        rebuild = toApp website.ci.rebuild;
        watch = toApp website.ci.watch;
        rebuild-watch = toApp website.ci.rebuild-watch;
        clean = toApp website.ci.clean;
        site = toApp website.site-with-thirdparty;
        default = rebuild-watch;
      };
      devShell = pkgs.haskellPackages.shellFor {
        packages = p: [ website.site ];
        buildInputs = with pkgs.haskellPackages; website.site.buildInputs ++ [
          haskell-language-server
          cabal-install
        ];
      };
    }
  );
}
