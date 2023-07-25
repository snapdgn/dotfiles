# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
   time.timeZone = "Canada/Eastern";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Enable the X11 windowing system.
   services.xserver.enable = true;
   services.xserver.windowManager.bspwm.enable = true;

  # fonts
  fonts.fonts = with pkgs; [
     noto-fonts
     noto-fonts-cjk
     noto-fonts-emoji
     liberation_ttf
     fira-code
     fira-code-symbols
     dina-font
     proggyfonts
     (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" "Inconsolata" "Iosevka" "Overpass" ]; })
];


  

  # Configure keymap in X11
   services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable Unfree Packages
  nixpkgs.config.allowUnfree = true;

  # microcode update
  hardware.cpu.intel.updateMicrocode = true;

  # Enable sound.
   sound.enable = true;
   hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
   services.xserver.libinput.enable = true;

  # JACK support
    services.jack = {
    jackd.enable = true;
    # support ALSA only programs via ALSA JACK PCM plugin
    alsa.enable = false;
    # support ALSA only programs via loopback device (supports programs like Steam)
    loopback = {
      enable = true;
      # buffering parameters for dmix device to work with ALSA only semi-professional sound programs
      #dmixConfig = ''
      #  period_size 2048
      #'';
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
   users.users.snapdgn= {
     isNormalUser = true;
     extraGroups = [ "wheel" "networkmanager" "docker" "jackaudio" ]; # Enable ‘sudo’ for the user.
     packages = with pkgs; [
       firefox
       tree
     ];
   };

   # home-manager

#   home-manager.users.snapdgn = { pkgs, ... }: {
#    home.packages = [ pkgs.atool pkgs.httpie ];
#    home.stateVersion = "23.05";
#    programs.bash.enable = true;
#   };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = false;
    users.snapdgn = import /home/snapdgn/.config/nixpkgs/home.nix;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
   environment.systemPackages = with pkgs; [
     vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
     wget
     zoxide
     bspwm
     sxhkd
     git
     alacritty
     st
#     (st.overrideAttrs (oldAttrs: rec {
#      configFile = writeText "config.def.h" (builtins.readFile /home/snapdgn/.config/st/config.h);
#     }))
     polybar
     dunst
     eww
     feh
     flameshot
     picom
     rofi
     qemu
     cmake
     neovim
     tldr
     unzip
     zsh
     virt-manager
     xorg.xinit
     xorg.xorgserver
     xorg.xf86inputevdev
     xorg.xf86inputsynaptics
     xorg.xf86inputlibinput
     xorg.xf86videointel
     xfce.thunar
     neofetch
     exa
     bat
     python3
     duf
     hexyl
     diff-so-fancy
     ripgrep
     keepassxc
     betterlockscreen
     nitrogen
     netcat
     brightnessctl
     pcmanfm
     tmux
     chromium
     lxappearance
     # nix specific pkgs
     home-manager
     # optional pkgs
     discord
     vscode
     # muzik
     spotifyd
     mpd
     # dev
     rustup
     rust-analyzer
   ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # enable programs
  programs.zsh.enable = true;
  programs.nm-applet.enable = true;
  users.defaultUserShell = pkgs.zsh;
  # only change shell for one user
  # users.users.snapdgn.shell = pkgs.zsh;

  # List services that you want to enable:
   services.xserver.xkbOptions = "ctrl:swapcaps";
   console.useXkbConfig = true;

  # Enable the OpenSSH daemon.
   services.openssh.enable = true;
   
  # bluetooth
   hardware.bluetooth.enable = true;
   services.blueman.enable = true;

  # enable docker
   virtualisation.docker.enable = true;

  # nix settings
  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
    };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
   networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
   system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}

