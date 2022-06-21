{ pkgs , thirdparty }: with pkgs;
let

  #######################
  ## Utility functions ##
  #######################

  wrap = { paths ? [], vars ? {}, file ? null, script ? null, shell ? false, bin ? false, name ? "wrap" }:
    assert file != null || script != null ||
          abort "wrap needs 'file' or 'script' argument";
    with rec {
      set  = n: v: "--set ${escapeShellArg (escapeShellArg n)} " +
                    "'\"'${escapeShellArg (escapeShellArg v)}'\"'";
      args = (map (p: "--prefix PATH : ${p}/bin") paths) ++
             (builtins.attrValues (builtins.mapAttrs set vars));
      destination = if bin then "/bin/${name}" else "";
    };
    runCommand name
      {
        f = if file != null then file
            else (
              if shell then writeShellScript
              else writeScript) "${name}-unwrapped" script;
        buildInputs = [ makeWrapper ];
      }
      ''
        makeWrapper "$f" "$out"${destination} ${builtins.toString args}
      '';

  ##################
  ## Compile site ##
  ##################

  # Get third party dependencies
  thirdparty' = linkFarm "thirdparty" thirdparty;

  # Generate the basic site
  site = (haskellPackages.callCabal2nix "Site" "${./site-builder}" {});

  # Bundle third party dependencies with site
  site-with-thirdparty =
    symlinkJoin {
      name = "site-with-thirdparty";
      paths = [ site ];
      buildInputs = [ makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/site --set THIRDPARTY "${thirdparty'}"
      '';
    };

  #######################
  ## Generate commands ##
  #######################

  rebuild-website = wrap {
    name = "generate-website";
    paths = [ site-with-thirdparty git ];
    script = ''
      site rebuild
    '';
  };

  watch-website = wrap {
    name = "watch-website";
    paths = [ site-with-thirdparty git ];
    script = ''
      site watch
    '';
  };

  rebuild-watch-website = wrap {
    name = "rebuild-watch-website";
    paths = [ site-with-thirdparty git ];
    script = ''
      site clean
      site watch
    '';
  };

  clean-website = wrap {
    name = "clean-website";
    paths = [ site-with-thirdparty git ];
    script = ''
      site clean
    '';
  };

  ###########
  ## Shell ##
  ###########

  haskell-env = haskellPackages.ghcWithHoogle (
    hp: with hp; [ 
      haskell-language-server 
      cabal-install 
    ]
    ++ site.buildInputs
  );

  shell = mkShell {
    name = "blog-env";
    buildInputs = [
      haskell-env
    ];
  };

in {
  inherit shell site site-with-thirdparty generate-website;
  ci = {
    rebuild = rebuild-website;
    watch = watch-website;
    rebuild-watch = rebuild-watch-website;
    clean = clean-website;
  };
}
