{
  description = "Website flake for aas.sh";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    bulma = {
      url = "git+https://github.com/jgthms/bulma/";
      flake = false;
    };
    uikit = {
      url = "https://github.com/uikit/uikit/archive/v3.6.20.tar.gz";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, bulma, uikit }:
    flake-utils.lib.eachDefaultSystem ( system:
      let
        pkgs' = import nixpkgs { inherit system; };
        website = pkgs'.callPackage ./site.nix {
          pkgs = pkgs';
          thirdparty = [
            {
              name = "bulma";
              path = "${bulma}/sass";
            }
            {
              name = "uikit";
              path = "${uikit}/src";
            }
          ];
        };
      in rec {
        defaultApp = apps.rebuild-watch;
        defaultPackage = website.site-with-thirdparty;
        devShell = website.shell;

        apps = {
          rebuild = flake-utils.lib.mkApp { 
            drv = website.ci.rebuild; 
            exePath = ""; 
          };
          watch = flake-utils.lib.mkApp { 
            drv = website.ci.watch; 
            exePath = ""; 
          };
          rebuild-watch = flake-utils.lib.mkApp { 
            drv = website.ci.rebuild-watch; 
            exePath = ""; 
          };
          clean = flake-utils.lib.mkApp { 
            drv = website.ci.clean; 
            exePath = ""; 
          };
          site = flake-utils.lib.mkApp { 
            drv = website.site-with-thirdparty; 
            exePath = "/bin/site"; 
          };
        };

        packages = { 
          inherit (website)
          site
          site-with-thirdparty 
          ci 
          shell; 
        };
      }
    );
}
