{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "snapdgn";
  home.homeDirectory = "/home/snapdgn";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

    home.packages = with pkgs; [
     wpsoffice
     slack
     zoom-us
     clang
     clang-tools
     fish
    # neomutt deps 
     neomutt
     pass
     mutt-wizard

    #haskell
    cabal-install
    ghc

    #misc
    zathura
    spotifyd

    # python
    python311Packages.python-lsp-server
    python311Packages.ipython

    # Language Servers
#    rust-analyzer
    nodePackages.typescript-language-server
    nodePackages_latest.eslint
    lua-language-server
    haskell-language-server
    gopls
    clang-tools
    nodePackages_latest.pyright
    nodePackages_latest.bash-language-server

    # rust packages

    wasm-pack
    cargo-generate
    wasm-bindgen-cli

    # dev
    vscode
    gef
    tshark

    #neomutt dep ends
     #llvmPackages_rocm.clang-tools-extra
  ];

  
   programs = {
     direnv.enable = true;
#     nix-direnv.enable = true;
  };

# -- Zsh config starts
  programs.zsh = {
  enable = false;
  shellAliases = {
    ll = "exa --icons -a";
    ls = "exa --icons";
    gc = "git clone";
    v = "nvim";
    cls = "clear";
    cd = "z";
    cat = "bat";
    bp = "bpython";
    df = "duf";
    xxd = "hexyl";
    lg = "lazygit";
    diff = "diff-so-fancy";
    k = "kubectl";
    ipy = "ipython";
    vt = "vagrant";
    grep = "rg";
    update = "sudo nixos-rebuild switch";
  };

  history = {
    size = 10000;
    path = "${config.xdg.dataHome}/zsh/history";
  };

  initExtra = ''
      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh 
    '';

   zplug = {
    enable = true;
    plugins = [
      { name = "zsh-users/zsh-autosuggestions"; } # Simple plugin installation
      { name = "romkatv/powerlevel10k"; tags = [ as:theme depth:1 ]; } # Installations with additional options. For the list of options, please refer to Zplug README.
    ];
  };

};
# --- Zsh config end------
  programs.zoxide = {
    enable = true;
  };

}
