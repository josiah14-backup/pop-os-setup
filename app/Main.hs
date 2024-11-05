-- The Miniconda3 is not able to be automated due to the way the
-- installation script is implemented.  Please install Miniconda
-- manually before running this script.
-- https://docs.conda.io/en/latest/miniconda.html#linux-installers
--
-- Creating the gpg key and registering it with pass for Racher Desktop
-- is also something I could not automate. To do that, run these commands:
-- gpg --generate-key
-- pass init <the hash for the .rev filename output by the previous command>
-- sysctl also needs to be altered to permit unprotected traffic on port 80
-- sudo echo \"net.ipv4.ip_unprivileged_port_start=80\" >> /etc/sysctl.conf
--
-- because of the CLI GUI of the installer, rkhunter needs to be manually
-- installed.
--
-- The Mirage Matrix client requires a manual download and is more effort than
-- it's worth to automate the install. Follow the directions here:
-- https://github.com/mirukana/mirage/blob/master/docs/INSTALL.md#flatpak
--
-- This installation also does not currently handle system theming
-- as I regard it as non-essential to the system and a rather complex
-- process to take the time to script-up.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (sequence)
import qualified Data.Foldable as F (fold)
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE (map)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Turtle
import Turtle.Format (format, fp)
import Turtle.Line (textToLine, textToLines)
import Turtle.Shell (Shell, sh)

echoText :: Text -> IO ()
echoText text = fmap F.fold $ mapM echo $ textToLines text

echoWhichLocation :: Turtle.FilePath -> Text -> Line -> IO ()
echoWhichLocation loc prefixText errorText =
  echo $ fromMaybe errorText $ textToLine (prefixText <> format fp loc)

aptInstall :: Turtle.FilePath -> Text -> Text -> Line -> IO ()
aptInstall binName packageName foundPrefix foundErrText =
  which binName
    >>= \case
      Nothing ->
        shell ("sudo apt install -y " <> packageName) empty
          >>= \case
            ExitSuccess -> return ()
            ExitFailure _ ->
              die ("ERROR: Could not install " <> pack binName)
      Just loc -> echoWhichLocation loc foundPrefix foundErrText

flatpakInstall :: Text -> IO ()
flatpakInstall applicationId =
  shellStrictWithErr ("flatpak info " <> applicationId) empty
    >>= \case
      (ExitSuccess, stdOutText, stdErrText) ->
        echoText (stdOutText <> stdErrText)
      (ExitFailure _, stdOutText, stdErrText) ->
        shell ("flatpak install --user -y " <> applicationId) empty
          >>= \case
            ExitSuccess -> echoText (stdOutText <> stdErrText)
            ExitFailure _ ->
              echoText (stdOutText <> stdErrText)
                >> die ("ERROR: Could not install " <> applicationId)

snapInstall :: Text -> Turtle.FilePath -> Text -> Text -> Line -> IO ()
snapInstall channelName binName packageName foundPrefix foundErrText =
  which binName
    >>= \case
      Nothing ->
        let cmd =
              pack "sudo snap install --color=always --"
                <> F.fold (
                        intersperse
                          " "
                          [ channelName,
                            packageName
                          ]
                    )
          in shell cmd empty
              >>= \case
                ExitSuccess -> return ()
                ExitFailure _ ->
                  die ("ERROR: Could not install " <> packageName)
      Just loc -> echoWhichLocation loc foundPrefix foundErrText

-- echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
-- echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
-- curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo -H gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
-- sudo chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg
-- sudo apt-get update
-- sudo apt-get install sbt

installSbt :: IO ()
installSbt = do
  sbtInstalled <- which "sbt"
  case sbtInstalled of
    Just sbtLoc ->
      echoWhichLocation
        sbtLoc
        "sbt already installed at "
        "sbt already installed."
    Nothing -> do
      shells
        "echo \"deb https://repo.scala-sbt.org/scalasbt/debian all main\" \
        \| sudo tee /etc/apt/sources.list.d/sbt.list"
        empty
      >> shells
        "echo \"deb https://repo.scala-sbt.org/scalasbt/debian /\" \
        \| sudo tee /etc/apt/sources.list.d/sbt_old.list"
        empty
      >> shells
        "curl -sL \"https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823\" \
        \| sudo -H gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import"
        empty
      >> shells
        "sudo chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg"
        empty
      >> shells
        "sudo apt update -y && sudo apt install -y sbt"
        empty

installBraveBrowser :: IO ()
installBraveBrowser = do
  -- Need to pipe some things together to get Brave to download correctly
  -- https://brave-browser.readthedocs.io/en/latest/installing-brave.html#linux
  braveInstalled <- which "brave-browser"
  case braveInstalled of
    Just braveLoc ->
      echoWhichLocation
        braveLoc
        "Brave Browser already install at "
        "Brave Browser already installed."
    Nothing -> do
      braveGpgKeyInstalled <-
        shellStrictWithErr
          "sudo apt-key --keyring \
          \ /etc/apt/trusted.gpg.d/brave-browser-release.gpg add -"
          $ inshell
            "curl -s \
            \ https://brave-browser-apt-release.s3.brave.com/brave-core.asc"
            empty
      case braveGpgKeyInstalled of
        (ExitSuccess, stdOutText, stdErrText) -> do
          braveAptSourcesListInstalled <-
            shellStrictWithErr
              "sudo tee \
              \ /etc/apt/sources.list.d/brave-browser-release.list"
              $ inshell
                "echo \"deb [arch=amd64] \
                \ https://brave-browser-apt-release.s3.brave.com/ \
                \ stable main\""
                empty
          case braveAptSourcesListInstalled of
            (ExitSuccess, stdOutText, stdErrText) ->
              shells "sudo apt -y update" empty
                >> aptInstall
                  "brave-browser"
                  "brave-browser"
                  "Brave Browser already installed at "
                  "Brave Browser already installed."
            (ExitFailure _, stdOutText, stdErrText) ->
              echoText (stdOutText <> stdErrText)
                >> die "ERROR: Could not add Brave repository to apt sources.list."
          echoText (stdOutText <> stdErrText)
        (ExitFailure _, stdOutText, stdErrText) ->
          echoText (stdOutText <> stdErrText)
            >> die "ERROR: Could not add Brave ASC to apt trusted GPG Keys."

installIceSsb :: IO ()
installIceSsb =
  which "ice"
    >>= \case
      Just iceLoc ->
        echoWhichLocation
          iceLoc
          "Ice SSB already installed at "
          "Ice SSB already installed."
      Nothing ->
        shells
          "sudo apt -y install python3-bs4 python3-soupsieve python3-lxml \
          \ && mkdir ice-deb && cd ice-deb && git clone https://github.com/peppermintos/ice.git \
          \ && cd ice && debuild --no-lintian -d -us -uc && sudo dpkg -i ../ice_*.deb && cd ../.. && rm -rf ice-deb \
          \ && sudo apt --fix-broken install -y"
          empty

installRustLang :: IO ()
installRustLang =
  which "rustup"
    >>= \case
      Just rustupLoc ->
        echoWhichLocation
          rustupLoc
          "Rustup already installed at "
          "Rustup already installed."
      Nothing -> do
        rustupInstallSuccessful <-
          shellStrictWithErr "sh -s -- -y" $
            inshell
              "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs"
              empty
        case rustupInstallSuccessful of
          (ExitSuccess, stdOutText, stdErrText) ->
            echoText (stdOutText <> stdErrText) >> echo "Rustup Installed."
          (ExitFailure _, stdOutText, stdErrText) ->
            echoText (stdOutText <> stdErrText)
              >> die "ERROR: Could not install Rustup"

installHaskellToolchain :: IO ()
installHaskellToolchain =
  which "ghcup"
    >>= \case
      Just ghcupLoc ->
        echoWhichLocation
          ghcupLoc
          "GHCup already installed at "
          "GHCup already installed."
      Nothing -> do
        ghcupInstallSuccessful <-
          shellStrictWithErr "BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=latest BOOTSTRAP_HASKELL_CABAL_VERSION=latest BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 BOOTSTRAP_HASKELL_ADJUST_BASHRC=P sh -s -- -y" $
            inshell
              "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org"
              empty
        case ghcupInstallSuccessful of
          (ExitSuccess, stdOutText, stdErrText) ->
            echoText (stdOutText <> stdErrText) >> echo "GHCup Installed."
          (ExitFailure _, stdOutText, stdErrText) ->
            echoText (stdOutText <> stdErrText)
              >> die "ERROR: Could not install GHCup"

installOhMyZsh :: IO ()
installOhMyZsh =
  (home >>= testpath . flip (</>) ".oh-my-zsh")
    >>= ( \case
            True -> echo "Oh My Zsh already installed."
            False ->
              shell
                "sh -c \"$(\
                \curl -fsSL \
                \https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh\
                \)\""
                empty
                >>= ( \case
                          ExitSuccess ->
                            shell "sudo usermod --shell $(which zsh) josiah" empty
                              >> echo "Oh My Zsh Install Successful"
                          ExitFailure _ -> echo "Oh My Zsh Install Failed."
                    )
        )

installOhMyZshPlugins :: IO ()
installOhMyZshPlugins = do
  zshCustomPluginsDir <- fmap (</> ".oh-my-zsh/custom/plugins") home

  zshAutosuggestionsInstalled <-
    testpath
      ( zshCustomPluginsDir </> "zsh-autosuggestions" )
  if not zshAutosuggestionsInstalled then
    shells
      "git clone https://github.com/zsh-users/zsh-autosuggestions \
      \ ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions"
      empty
  else
    echo "Zsh-Autosuggestions already installed"

  zshSyntaxHighlightingInstalled <-
    testpath
      ( zshCustomPluginsDir </> "zsh-syntax-highlighting" )
  if not zshSyntaxHighlightingInstalled then
    shells
      "git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
      \ ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
      empty
  else
    echo "Zsh-Syntax-Highlighting already installed."

  nixZshCompletionsInstalled <-
    testpath
      ( zshCustomPluginsDir </> "nix-zsh-completions" )
  if not nixZshCompletionsInstalled then
    shells
      "git clone https://github.com/nix-community/nix-zsh-completions.git \
      \ ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/nix-zsh-completions"
      empty
  else
    echo "Nix-Zsh-Completions already installed."

  nixShellInstalled <-
    testpath
      ( zshCustomPluginsDir </> "nix-shell" )
  if not nixShellInstalled then
    shells
      "git clone https://github.com/chisui/zsh-nix-shell.git \
      \ ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/nix-shell"
      empty
  else
    echo "Nix-Shell ZSH plugin already installed."

copyDotFilesToHome :: IO ()
copyDotFilesToHome = do
  homedir <- home
  curdir <- pwd
  cp (curdir </> ".profile") (homedir </> ".profile")
  cp (curdir </> ".bashrc") (homedir </> ".bashrc")
  cp (curdir </> ".bash_profile") (homedir </> ".bash_profile")
  cp (curdir </> ".zshrc") (homedir </> ".zshrc")
  cp (curdir </> ".zprofile") (homedir </> ".zprofile")
  cp (curdir </> ".tmux.conf") (homedir </> ".tmux.conf")
  cp (curdir </> ".gitconfig") (homedir </> ".gitconfig")
  cptree (curdir </> ".xmonad") (homedir </> ".xmonad")

installPowerline :: IO ()
installPowerline =
  shellStrictWithErr "pip show powerline-status" empty
    >>= \case
      (ExitSuccess, stdOutText, stdErrText) ->
        echoText (stdOutText <> stdErrText) >> echo "Powerline already installed."
      (ExitFailure _, stdOutText, stdErrText) ->
        echoText (stdOutText <> stdErrText)
          >> shells "pip install powerline-status" empty
          >> shells "git clone https://github.com/powerline/fonts.git" empty
          >> cd "fonts"
          >> shells "sh ./install.sh" empty
          >> cd ".."
          >> rmtree "fonts"

installDocker :: IO ()
installDocker = do
  dockerInstalled <- which "docker"
  case dockerInstalled of
    Just dockerLoc ->
      echoWhichLocation
        dockerLoc
        "Docker already installed at "
        "Docker already installed."
    Nothing ->
      shells "sudo apt-key add -" (
          inshell "curl -fsSL https://download.docker.com/linux/ubuntu/gpg" empty
      )
        >> shells
          "sudo apt-add-repository \
          \ \"deb [arch=amd64] https://download.docker.com/linux/ubuntu \
          \ $(lsb_release -cs) \
          \ stable\""
          empty
        >> shells "sudo apt -y update" empty
        >> shells
          "sudo apt install -y docker docker-ce docker-ce-cli containerd.io \
          \docker-compose"
          empty

installKompose :: IO ()
installKompose =
  which "kompose" >>= \case
    Just komposeLoc ->
      echoWhichLocation
        komposeLoc
        "Kompose already installed at "
        "Kompose already installed."
    Nothing ->
      shells
        "curl -L https://github.com/kubernetes/kompose/releases/download/v1.34.0/kompose-linux-amd64 -o kompose"
        empty
        >> chmod executable "./kompose"
        >> shells "sudo mv ./kompose /usr/local/bin/kompose" empty

installKinD :: IO ()
installKinD =
  which "kind" >>= \case
    Just kindLoc ->
      echoWhichLocation
        kindLoc
        "KinD already installed at "
        "KinD already installed."
    Nothing ->
      shells
        "curl -Lo ./kind https://kind.sigs.k8s.io/dl/v0.24.0/kind-linux-amd64"
        empty
        >> chmod executable "./kind"
        >> shells "sudo mv ./kind /usr/local/bin/kind" empty

installRancherDesktop :: IO ()
installRancherDesktop =
  which "rancher-desktop" >>= \case
    Just rancherLoc ->
      echoWhichLocation
        rancherLoc
        "Rancher Desktop already installed at "
        "Rancher Desktop already installed."
    Nothing ->
      shells
        "curl -s https://download.opensuse.org/repositories/isv:/Rancher:/stable/deb/Release.key \
        \| gpg --dearmor \
        \| sudo dd status=none of=/usr/share/keyrings/isv-rancher-stable-archive-keyring.gpg \
        \&& echo 'deb [signed-by=/usr/share/keyrings/isv-rancher-stable-archive-keyring.gpg] https://download.opensuse.org/repositories/isv:/Rancher:/stable/deb/ ./' \
        \| sudo dd status=none of=/etc/apt/sources.list.d/isv-rancher-stable.list"
        empty
      >> shells "sudo usermod -a -G kvm \"$USER\"" empty
      >> shells "sudo apt -y update && sudo apt -y install rancher-desktop" empty
      >> shells "sudo sysctl -w net.ipv4.ip_unprivileged_port_start=80" empty

installTerraform :: IO ()
installTerraform =
  which "terraform" >>= \case
    Just terraformLoc ->
      echoWhichLocation
        terraformLoc
        "Terraform already installed at "
        "Terrafrom already installed."
    Nothing ->
      shells
        "wget -O- https://apt.releases.hashicorp.com/gpg \
        \| gpg --dearmor \
        \| sudo tee /usr/share/keyrings/hashicorp-archive-keyring.gpg > /dev/null"
        empty
      >> shells
        "echo \"deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] \
        \https://apt.releases.hashicorp.com $(lsb_release -cs) main\" \
        \| sudo tee /etc/apt/sources.list.d/hashicorp.list"
        empty
      >> shells "sudo apt -y update" empty
      >> shells "sudo apt install -y terraform" empty

installKubectl :: IO ()
installKubectl =
  which "kubectl" >>= \case
    Just kubectlLoc ->
      echoWhichLocation
        kubectlLoc
        "kubectl already installed at "
        "kubectl already installed."
    Nothing ->
      shells
        "curl -fsSL https://pkgs.k8s.io/core:/stable:/v1.31/deb/Release.key \
        \| sudo gpg --dearmor -o /etc/apt/keyrings/kubernetes-apt-keyring.gpg \
        \&& sudo chmod 644 /etc/apt/keyrings/kubernetes-apt-keyring.gpg"
        empty
        >> shells "sudo tee -a /etc/apt/sources.list.d/kubernetes.list" (
                inshell
                  "echo 'deb [signed-by=/etc/apt/keyrings/kubernetes-apt-keyring.gpg] https://pkgs.k8s.io/core:/stable:/v1.31/deb/ /'"
                  empty
            )
        >> shells "sudo chmod 644 /etc/apt/sources.list.d/kubernetes.list" empty
        >> shells "sudo apt -y update" empty
        >> shells "sudo apt install -y kubectl" empty

prepareAzureCliInstall :: IO ()
prepareAzureCliInstall =
  which "az" >>= \case
    Just azLoc ->
      echoWhichLocation
        azLoc
        "Azure CLI already installed at "
        "Azure CLI already installed."
    Nothing ->
      shells
        "curl -sL https://packages.microsoft.com/keys/microsoft.asc \
        \ | gpg --dearmor \
        \ | sudo tee /etc/apt/trusted.gpg.d/microsoft.gpg > /dev/null"
        empty
        >> shells "sudo tee /etc/apt/sources.list.d/azure-cli.list" (
                inshell
                  "echo \"deb [arch=amd64] \
                  \ https://packages.microsoft.com/repos/azure-cli/ \
                  \ $(lsb_release -cs) main\""
                  empty
            )
        >> shells "sudo apt update -y" empty
        >> shells "sudo apt install -y azure-cli" empty

installPythonPoetry :: IO ()
installPythonPoetry =
  which "poetry" >>= \case
    Just poetryLoc ->
      echoWhichLocation
        poetryLoc
        "Poetry package manager for Python already installed at "
        "Poetry package manager for Python already installed."
    Nothing ->
      shells "python3 -" (
          inshell
            "curl -sSL \
            \https://install.python-poetry.org"
            empty
      )
        >> shells "echo 'export PATH=\"${HOME}/.poetry/bin:${PATH}\"' >> ~/.zprofile" empty

main :: IO ()
main = do
  shell "sudo apt -y update && sudo apt -y upgrade" empty
  aptInstall
    "curl"
    "curl"
    "cURL already installed at "
    "cURL already installed."
  shells
    "sudo apt install -y apt-transport-https ca-certificates devscripts gnupg-agent \
    \build-essential autoconf libffi7 libgmp10 libncurses5 libtinfo5 \
    \software-properties-common libgtk-3-dev libicu-dev libncurses-dev \
    \libgmp-dev zlib1g-dev libtinfo-dev libc6-dev libffi-dev g++ gcc make \
    \xz-utils gnupg gnupg2 libbz2-dev python3-tk liblzma-dev lzma-dev linux-headers-$(uname -r)"
    empty
  aptInstall
    "ansible"
    "ansible"
    "ansible already installed at "
    "ansible already installed."
  aptInstall
    "herbstluftwm"
    "herbstluftwm"
    "herbstluftwm already installed at "
    "herbstluftwm already installed."
  aptInstall
    "blt-dev"
    "blt-dev"
    "blt-dev already installed at "
    "blt-dev already installed."
  aptInstall
    "blueman"
    "blueman"
    "blueman already installed at "
    "blueman already installed."
  aptInstall
    "bluez-tools"
    "bluez-tools"
    "bluez-tools already installed at "
    "bluez-tools already installed."
  aptInstall
    "bridge-utils"
    "bridge-utils"
    "bridge-utils already installed at "
    "bridge-utils already installed."
  aptInstall
    "bzip2"
    "bzip2"
    "bzip2 already installed at "
    "bzip2 already installed."
  aptInstall
    "cheese"
    "cheese"
    "cheese already installed at "
    "cheese already installed."
  aptInstall
    "chrome-gnome-shell"
    "chrome-gnome-shell"
    "chrome-gnome-shell already installed at "
    "chrome-gnome-shell already installed."
  aptInstall
    "cmake"
    "cmake"
    "cmake already installed at "
    "cmake already installed."
  aptInstall
    "dash"
    "dash"
    "dash already installed at "
    "dash already installed."
  aptInstall
    "dctrl-tools"
    "dctrl-tools"
    "dctrl-tools already installed at "
    "dctrl-tools already installed."
  aptInstall
    "debhelper"
    "debhelper"
    "debhelper already installed at "
    "debhelper already installed."
  aptInstall
    "dialog"
    "dialog"
    "dialog already installed at "
    "dialog already installed."
  aptInstall
    "dmeventd"
    "dmeventd"
    "dmeventd already installed at "
    "dmeventd already installed."
  aptInstall
    "expect"
    "expect"
    "expect already installed at "
    "expect already installed."
  aptInstall
    "fd-find"
    "fd-find"
    "fd-find already installed at "
    "fd-find already installed."
  aptInstall
    "fdisk"
    "fdisk"
    "fdisk already installed at "
    "fdisk already installed."
  aptInstall
    "file"
    "file"
    "file already installed at "
    "file already installed."
  aptInstall
    "firefox"
    "firefox"
    "firefox already installed at "
    "firefox already installed."
  aptInstall
    "firejail"
    "firejail"
    "firejail already installed at "
    "firejail already installed."
  aptInstall
    "firejail-profiles"
    "firejail-profiles"
    "firejail-profiles already installed at "
    "firejail-profiles already installed."
  aptInstall
    "firetools"
    "firetools"
    "firetools already installed at "
    "firetools already installed."
  aptInstall
    "fonts-arabeyes"
    "fonts-arabeyes"
    "fonts-arabeyes already installed at "
    "fonts-arabeyes already installed."
  aptInstall
    "fonts-kacst"
    "fonts-kacst"
    "fonts-kacst already installed at "
    "fonts-kacst already installed."
  aptInstall
    "xclip"
    "xclip"
    "xclip already installed at "
    "xclip already installed."
  aptInstall
    "snap"
    "snapd"
    "Snappy already installed at "
    "Snappy already installed."
  aptInstall
    "ecryptfs"
    "ecryptfs-utils"
    "eCryptFS already installed at "
    "eCryptFS already installed."
  aptInstall
    "srm"
    "secure-delete"
    "Secure Delete already installed at "
    "Secure Delete already installed."
  aptInstall
    "git"
    "git"
    "Git already installed at "
    "Git already installed."
  aptInstall
    "gnome-settings-daemon"
    "gnome-settings-daemon"
    "gnome-settings-daemon already installed at "
    "gnome-settings-daemon already installed."
  aptInstall
    "gnome-shell-extension-desktop-icons-ng"
    "gnome-shell-extension-desktop-icons-ng"
    "gnome-shell-extension-desktop-icons-ng already installed at "
    "gnome-shell-extension-desktop-icons-ng already installed."
  aptInstall
    "gnome-themes-extra"
    "gnome-themes-extra"
    "gnome-themes-extra already installed at "
    "gnome-themes-extra already installed."
  shells
    "sudo apt install -y gstreamer1.0-alsa gstreamer1.0-clutter-3.0 gstreamer1.0-gl \
    \gstreamer1.0-gtk3 gstreamer1.0-packagekit gstreamer1.0-pipewire gstreamer1.0-plugins-base \
    \gstreamer1.0-plugins-base-apps gstreamer1.0-plugins-good gstreamer1.0-pulseaudio gstreamer1.0-tools \
    \gstreamer1.0-x libgstreamer1.0-0 \
    \gstreamer1.0-adapter-pulseeffects gstreamer1.0-autogain-pulseeffects gstreamer1.0-convolver-pulseeffects gstreamer1.0-crystalizer-pulseeffects \
    \gstreamer1.0-espeak gstreamer1.0-libav gstreamer1.0-nice \
    \gstreamer1.0-omx-bellagio-config gstreamer1.0-omx-generic gstreamer1.0-omx-generic-config \
    \gstreamer1.0-opencv gstreamer1.0-pocketsphinx gstreamer1.0-python3-plugin-loader \
    \gstreamer1.0-qt5 gstreamer1.0-rtsp gstreamer1.0-vaapi gstreamer1.0-wpe gstreamer1.0-fdkaac"
    empty
  aptInstall
    "gtk2-engines-murrine"
    "gtk2-engines-murrine"
    "gtk2-engines-murrine already installed at "
    "gtk2-engines-murrine already installed."
  aptInstall
    "htop"
    "htop"
    "htop already installed at "
    "htop already installed."
  aptInstall
    "i2p"
    "i2p"
    "i2p already installed at "
    "i2p already installed."
  aptInstall
    "i965-va-driver"
    "i965-va-driver"
    "i965-va-driver already installed at "
    "i965-va-driver already installed."
  aptInstall
    "ibus-mozc"
    "ibus-mozc"
    "ibus-mozc already installed at "
    "ibus-mozc already installed."
  installIceSsb
  aptInstall
    "inkscape"
    "inkscape"
    "inkscape already installed at "
    "inkscape already installed."
  aptInstall
    "intel-media-va-driver-non-free"
    "intel-media-va-driver-non-free"
    "intel-media-va-driver-non-free already installed at "
    "intel-media-va-driver-non-free already installed."
  aptInstall
    "jq"
    "jq"
    "jq already installed at "
    "jq already installed."
  aptInstall
    "julia"
    "julia"
    "julia already installed at "
    "julia already installed."
  aptInstall
    "kpartx"
    "kpartx"
    "kpartx already installed at "
    "kpartx already installed."
  aptInstall
    "kpartx-boot"
    "kpartx-boot"
    "kpartx-boot already installed at "
    "kpartx-boot already installed."
  aptInstall
    "vim"
    "vim"
    "Vim already installed at "
    "Vim already installed."
  aptInstall
    "gvim"
    "vim-gtk3"
    "GVim already installed at "
    "GVim already installed."
  installHaskellToolchain
  aptInstall
    "lpass"
    "lastpass-cli"
    "LastPass CLI client already installed at "
    "LastPass CLI client already installed."
  aptInstall
    "lvm2"
    "lvm2"
    "lvm2 already installed at "
    "lvm2 already installed."
  aptInstall
    "mesa-utils"
    "mesa-utils"
    "mesa-utils already installed at "
    "mesa-utils already installed."
  aptInstall
    "mesa-va-drivers"
    "mesa-va-drivers"
    "mesa-va-drivers already installed at "
    "mesa-va-drivers already installed."
  aptInstall
    "mesa-va-drivers"
    "mesa-va-drivers"
    "mesa-va-drivers already installed at "
    "mesa-va-drivers already installed."
  aptInstall
    "nvidia-driver-560"
    "nvidia-driver-560"
    "nvidia-driver-560 already installed at "
    "nvidia-driver-560 already installed."
  aptInstall
    "nvidia-utils-560"
    "nvidia-utils-560"
    "nvidia-utils-560 already installed at "
    "nvidia-utils-560 already installed."
  aptInstall
    "nvidia-compute-utils-560"
    "nvidia-compute-utils-560"
    "nvidia-compute-utils-560 already installed at "
    "nvidia-compute-utils-560 already installed."
  aptInstall
    "nvidia-dkms-560"
    "nvidia-dkms-560"
    "nvidia-dkms-560 already installed at "
    "nvidia-dkms-560 already installed."
  aptInstall
    "xserver-xorg-video-nvidia-560"
    "xserver-xorg-video-nvidia-560"
    "xserver-xorg-video-nvidia-560 already installed at "
    "xserver-xorg-video-nvidia-560 already installed."
  aptInstall
    "nvidia-kernel-common-560"
    "nvidia-kernel-common-560"
    "nvidia-kernel-common-560 already installed at "
    "nvidia-kernel-common-560 already installed."
  aptInstall
    "nvidia-kernel-source-560"
    "nvidia-kernel-source-560"
    "nvidia-kernel-source-560 already installed at "
    "nvidia-kernel-source-560 already installed."
  aptInstall
    "psql"
    "postgresql-all"
    "postgresql already installed at "
    "postgresql already installed."
  aptInstall
    "pass"
    "pass"
    "pass already installed at "
    "pass already installed."
  aptInstall
    "qutebrowser"
    "qutebrowser"
    "qutebrowser already installed at "
    "qutebrowser already installed."
  installRancherDesktop
  aptInstall
    "rhythmbox"
    "rhythmbox"
    "rhythmbox already installed at "
    "rhythmbox already installed."
  aptInstall
    "sassc"
    "sassc"
    "sassc already installed at "
    "sassc already installed."
  installSbt
  aptInstall
    "scrot"
    "scrot"
    "scrot already installed at "
    "scrot already installed."
  aptInstall
    "suckless-tools"
    "suckless-tools"
    "suckless-tools already installed at "
    "suckless-tools already installed."
  aptInstall
    "steam"
    "steam"
    "steam already installed at "
    "steam already installed."
  aptInstall
    "system76-cuda-latest"
    "system76-cuda-latest"
    "system76-cuda-latest already installed at "
    "system76-cuda-latest already installed."
  aptInstall
    "system76-driver"
    "system76-driver"
    "system76-driver already installed at "
    "system76-driver already installed."
  aptInstall
    "system76-driver-nvidia"
    "system76-driver-nvidia"
    "system76-driver-nvidia already installed at "
    "system76-driver-nvidia already installed."
  aptInstall
    "system76-firmware"
    "system76-firmware"
    "system76-firmware already installed at "
    "system76-firmware already installed."
  aptInstall
    "system76-scheduler"
    "system76-scheduler"
    "system76-scheduler already installed at "
    "system76-scheduler already installed."
  aptInstall
    "tlp"
    "tlp"
    "tlp already installed at "
    "tlp already installed."
  aptInstall
    "tlp-rdw"
    "tlp-rdw"
    "tlp-rdw already installed at "
    "tlp-rdw already installed."
  aptInstall
    "vifm"
    "vifm"
    "Vifm already installed at "
    "Vifm already installed."
  aptInstall
    "va-driver-all"
    "va-driver-all"
    "va-driver-all already installed at "
    "va-driver-all already installed."
  aptInstall
    "sensors"
    "lm-sensors"
    "Hardware Sensors CLI program already installed at "
    "Hardware Sensors CLI program already installed."
  aptInstall
    "xdotool"
    "xdotool"
    "xdotool already installed at "
    "xdotool already installed."
  aptInstall
    "screenfetch"
    "screenfetch"
    "screenfetch already installed at "
    "screenfetch already installed."
  aptInstall
    "gnome-tweaks"
    "gnome-tweaks"
    "Gnome Tweaks already installed at "
    "Gnome Tweaks already installed."
  installBraveBrowser
  aptInstall
    "tmux"
    "tmux"
    "Tmux already installed at "
    "Tmux already installed."
  aptInstall "zsh" "zsh" "ZSH already installed at " "ZSH already installed."
  installOhMyZsh
  aptInstall
    "ag"
    "silversearcher-ag"
    "Silver Searcher already installed at "
    "Silver Searcher already installed."
  aptInstall
    "rg"
    "ripgrep"
    "Ripgrep already installed at "
    "Ripgrep already installed."
  installRustLang
  aptInstall
    "go"
    "golang"
    "Go already installed at "
    "Go already installed."
  aptInstall "npm" "npm" "NPM already installed at " "NPM already installed."
  aptInstall
    "python3"
    "python3"
    "Python 3.8 already installed at "
    "Python 3.8 already installed."
  aptInstall
    "python3-pip"
    "pip"
    "Pip already installed at "
    "Pip already installed."
  shell
    "sudo update-alternatives --install \
    \ /usr/bin/python python /usr/bin/python3 1"
    empty
  installPythonPoetry
  installOhMyZshPlugins
  installPowerline
  copyDotFilesToHome
  nixInstalled <- which "nix-shell"
  case nixInstalled of
    Just nixShellLoc ->
      echoWhichLocation
        nixShellLoc
        "nix-shell found at "
        "nix-shell found."
    Nothing ->
      shells "sh" $
        inshell
          "curl -L https://nixos.org/nix/install"
          empty
  installDocker
  prepareAzureCliInstall
  aptInstall
    "az"
    "azure-cli"
    "Azure CLI already installed at "
    "Azure CLI already installed."
  aptInstall
    "kubernetes"
    "kubernetes"
    "K8S already installed at "
    "K8S already installed."
  installKubectl
  installKompose
  installIceSsb
  snapInstall
    "stable"
    "snapd"
    "snapd"
    "snapd already installed at "
    "snapd already installed."
  snapInstall
    "stable"
    "helm"
    "--classic helm"
    "K8S Helm already installed at "
    "K8S Helm already installed."
  snapInstall
    "stable"
    "bitwarden"
    "bitwarden"
    "BitWarden already installed at "
    "BitWarden already installed."
  snapInstall
    "stable"
    "bw"
    "bw"
    "BitWarden CLI already installed at "
    "BitWarden CLI already installed."
  snapInstall
    "stable"
    "aetherp2p"
    "aetherp2p"
    "Aetherp2p already installed at "
    "Aetherp2p already installed."
  snapInstall
    "stable"
    "bare"
    "bare"
    "bare already installed at "
    "bare already installed."
  snapInstall
    "stable"
    "brave"
    "brave"
    "Brave Browser already installed at "
    "Brave Browser already installed."
  snapInstall
    "stable"
    "feedreader"
    "feedreader"
    "feedreader already installed at "
    "feedreader already installed."
  snapInstall
    "stable"
    "cups"
    "cups"
    "cups already installed at "
    "cups already installed."
  snapInstall
    "stable"
    "glow"
    "glow"
    "glow already installed at "
    "glow already installed."
  snapInstall
    "stable"
    "joplin-desktop"
    "joplin-desktop"
    "joplin-desktop already installed at "
    "joplin-desktop already installed."
  snapInstall
    "stable"
    "microk8s"
    "--classic microk8s"
    "microk8s already installed at "
    "microk8s already installed."
  snapInstall
    "stable"
    "musescore"
    "musescore"
    "Musescore already installed at "
    "Musescore already installed."
  snapInstall
    "stable"
    "spotify"
    "spotify"
    "Spotify already installed at "
    "Spotify already installed."
  snapInstall
    "stable"
    "teams"
    "teams-for-linux"
    "Teams already installed at "
    "Teams already installed."
  snapInstall
    "stable"
    "emacs"
    "--classic emacs"
    "Emacs already installed at "
    "Emacs already installed."
  snapInstall
    "edge"
    "thefuck"
    "--classic thefuck"
    "thefuck already installed at "
    "thefuck already installed."
  installKinD
  installTerraform
  shells "sudo apt autoremove -y" empty
  shells "dconf load / < gnome-settings.dconf" empty
  addFlathubRemoteExitCode <-
    shell
      "flatpak remote-add --if-not-exists flathub \
      \ https://flathub.org/repo/flathub.flatpakrepo"
      empty
  addFlathubBetaRemoteExitCode <-
    shell
      "flatpak remote-add --if-not-exists flathub-beta \
      \ https://flathub.org/beta-repo/flathub-beta.flatpakrepo"
      empty
  case (addFlathubRemoteExitCode, addFlathubBetaRemoteExitCode) of
    (ExitSuccess, ExitSuccess) ->
      flatpakInstall "ca.desrt.dconf-editor"
        >> flatpakInstall "com.bitwarden.desktop"
        >> flatpakInstall "com.brave.Browser"
        >> flatpakInstall "com.discordapp.Discord"
        >> flatpakInstall "com.github.Eloston.UngoogledChromium"
        >> flatpakInstall "com.github.Eloston.UngoogledChromium.Codecs"
        >> flatpakInstall "com.github.inercia.k3x"
        >> flatpakInstall "com.jetbrains.PyCharm-Community"
        >> flatpakInstall "com.microsoft.Teams"
        >> flatpakInstall "com.nextcloud.desktopclient.nextcloud"
        >> flatpakInstall "com.obsproject.Studio"
        >> flatpakInstall "com.protonvpn.www"
        >> flatpakInstall "com.slack.Slack"
        >> flatpakInstall "com.spotify.Client"
        >> flatpakInstall "com.sublimetext.three"
        >> flatpakInstall "com.valvesoftware.Steam"
        >> flatpakInstall "dev.alextren.Spot"
        >> flatpakInstall "engineer.atlas.Nyxt"
        >> flatpakInstall "im.fluffychat.Fluffychat"
        >> flatpakInstall "im.riot.Riot"
        >> flatpakInstall "io.lbry.lbry-app"
        >> flatpakInstall "io.thp.numptyphysics"
        >> flatpakInstall "md.obsidian.Obsidian"
        >> flatpakInstall "net.cozic.joplin_desktop"
        >> flatpakInstall "network.loki.Session"
        >> flatpakInstall "org.electrum.electrum"
        >> flatpakInstall "org.gimp.GIMP"
        >> flatpakInstall "org.gimp.GIMP.Manual"
        >> flatpakInstall "org.gnome.Fractal"
        >> flatpakInstall "org.gnome.Rhythmbox3"
        >> flatpakInstall "org.gnome.Solanum"
        >> flatpakInstall "org.mozilla.firefox"
        >> flatpakInstall "org.musescore.MuseScore"
        >> flatpakInstall "org.remmina.Remmina"
        >> flatpakInstall "org.signal.Signal"
        >> flatpakInstall "org.telegram.desktop"
        >> flatpakInstall "org.telegram.desktop.webview"
        >> flatpakInstall "us.zoom.Zoom"
    (ExitFailure exitCode, ExitSuccess) -> die "Could not add the remote 'flathub'."
    (ExitSuccess, ExitFailure exitCode) -> die "Could not add the remote 'flathub-beta'."
    (ExitFailure exitCode0, ExitFailure exitCode1) -> die "Could not add ANY of the flatpak remotes."
  echo "DONE"
