{ pkgs , nur, thirdparty }: with pkgs;
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

  site = (haskellPackages.callCabal2nix "Site" "${./site-builder}" {});

  #######################
  ## Generate commands ##
  #######################

  generate-website = wrap {
    name = "generate-website";
    paths = [ site git ];

    script = ''
      site rebuild
    '';
  };

  watch-website = wrap {
    name = "watch-website";
    paths = [ site git ];

    script = ''
      site watch
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
  inherit shell site generate-website;
  ci = {
    compile = generate-website;
    watch = watch-website;
  };
}
