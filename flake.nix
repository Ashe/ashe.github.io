{
  description = "Website flake for aas.sh";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    nur.url = "github:nix-community/NUR";
    flake-utils.url = "github:numtide/flake-utils";
    uikit-src = {
      url = "https://github.com/uikit/uikit/archive/v3.6.20.tar.gz";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nur, flake-utils, uikit-src }:
    flake-utils.lib.eachDefaultSystem ( system:
      let
        pkgs' = import nixpkgs { inherit system; overlays = [ nur.overlay ]; };
        website = pkgs'.callPackage ./site.nix {
          pkgs = pkgs';
          nur = pkgs'.nur;
          thirdparty = [
            {
              name = "uikit";
              path = "${uikit-src}/src";
            }
          ];
        };
      in rec {
        defaultApp = apps.watch;
        defaultPackage = website.site-with-thirdparty;
        devShell = website.shell;

        apps = {
          compile = flake-utils.lib.mkApp { 
            drv = website.ci.compile; 
            exePath = ""; 
          };
          site = flake-utils.lib.mkApp { 
            drv = website.site-with-thirdparty; 
            exePath = "/bin/site"; 
          };
          watch = flake-utils.lib.mkApp { 
            drv = website.ci.watch; 
            exePath = ""; 
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
