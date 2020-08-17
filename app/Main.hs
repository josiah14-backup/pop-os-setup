-- The Miniconda3 is not able to be automated due to the way the
-- installation script is implemented.  Please install Miniconda
-- manually before running this script.
-- https://docs.conda.io/en/latest/miniconda.html#linux-installers
--
-- This installation also does not currently handle system theming
-- as I regard it as non-essential to the system and a rather complex
-- process to take the time to script-up.
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Turtle.Format (format, fp)
import Turtle.Line (textToLine, textToLines)
import Turtle.Shell (Shell, sh)

import qualified Data.Foldable as F (fold)
import Control.Monad (sequence)
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE (map)
import Data.Text (pack)
import Data.Maybe (fromMaybe)

echoText :: Text -> IO ()
echoText text = fmap F.fold $ sequence $ NE.map echo $ textToLines text

echoWhichLocation :: Turtle.FilePath -> Text -> Line -> IO ()
echoWhichLocation loc prefixText errorText =
  echo $ fromMaybe errorText $ textToLine (prefixText <> format fp loc)

aptInstall :: Turtle.FilePath -> Text -> Text -> Line -> IO ()
aptInstall binName packageName foundPrefix foundErrText =
  which binName
  >>= \installed -> 
        case installed of
          Nothing -> shell ("sudo apt install -y " <> packageName) empty
            >>= \installStatus ->
                  case installStatus of
                    ExitSuccess -> return ()
                    ExitFailure _ ->
                      die ("ERROR: Could not install "
                           <> (pack $ encodeString binName))
          Just loc -> echoWhichLocation loc foundPrefix foundErrText

flatpakInstall :: Text -> IO ()
flatpakInstall applicationId =
  shellStrictWithErr ("flatpak info " <> applicationId) empty
  >>= \installed ->
        case installed of
          (ExitSuccess, stdOutText, stdErrText) ->
            echoText (stdOutText <> stdErrText)
          (ExitFailure _, stdOutText, stdErrText) ->
            shell ("flatpak install --user -y " <> applicationId) empty
            >>= \installStatus ->
                  case installStatus of
                    ExitSuccess -> echoText (stdOutText <> stdErrText)
                    ExitFailure _ -> 
                      echoText (stdOutText <> stdErrText) 
                      >> die ("ERROR: Could not install " <> applicationId)

snapInstall :: Text -> Turtle.FilePath -> Text -> Text -> Line -> IO ()
snapInstall channelName binName packageName foundPrefix foundErrText =
  which binName
  >>= \installed ->
        case installed of
          Nothing ->
            let cmd = (pack "sudo snap install --color=always --")
                       <> (F.fold $ intersperse " "
                                                [ channelName
                                                , packageName])
            in shell cmd empty
               >>= \installStatus ->
                     case installStatus of
                       ExitSuccess -> return ()
                       ExitFailure _ ->
                         die ("ERROR: Could not install " <> packageName)
          Just loc -> echoWhichLocation loc foundPrefix foundErrText

installBraveBrowser :: IO ()
installBraveBrowser = do
  -- Need to pipe some things together to get Brave to download correctly
  -- https://brave-browser.readthedocs.io/en/latest/installing-brave.html#linux
  braveInstalled <- which "brave-browser"
  case braveInstalled of
    Just braveLoc -> 
      echoWhichLocation braveLoc
                        "Brave Browser already install at "
                        "Brave Browser already installed."
    Nothing -> do
      braveGpgKeyInstalled <- 
        shellStrictWithErr "sudo apt-key --keyring \
                          \ /etc/apt/trusted.gpg.d/brave-browser-release.gpg add -"
        $ inshell "curl -s \
                 \ https://brave-browser-apt-release.s3.brave.com/brave-core.asc"
                  empty
      case braveGpgKeyInstalled of
        (ExitSuccess, stdOutText, stdErrText) -> do
          braveAptSourcesListInstalled <-
            shellStrictWithErr "sudo tee \
                              \ /etc/apt/sources.list.d/brave-browser-release.list"
            $ inshell "echo \"deb [arch=amd64] \
                     \ https://brave-browser-apt-release.s3.brave.com/ \
                     \ stable main\""
                      empty
          case braveAptSourcesListInstalled of
            (ExitSuccess, stdOutText, stdErrText) ->
              shells "sudo apt -y update" empty
              >> aptInstall "brave-browser"
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

installRustLang :: IO ()
installRustLang = 
  which "rustup"
  >>= \rustupInstalled ->
   case rustupInstalled of
     Just rustupLoc ->
       echoWhichLocation rustupLoc
                         "Rustup already installed at "
                         "Rustup already installed."
     Nothing -> do
       rustupInstallSuccessful <-
         shellStrictWithErr "sh"
         $ inshell "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs"
                   empty
       case rustupInstallSuccessful of
         (ExitSuccess, stdOutText, stdErrText) -> 
           echoText (stdOutText <> stdErrText) >> echo "Rustup Installed."
         (ExitFailure _, stdOutText, stdErrText) ->
           echoText (stdOutText <> stdErrText)
           >> die "ERROR: Could not install Rustup"

installOhMyZsh :: IO ()
installOhMyZsh =
  fmap (flip (</>) ".oh-my-zsh") home >>= testpath >>= (
    \ohmyzshInstalled -> case ohmyzshInstalled of
      True -> echo "Oh My Zsh already installed."
      False ->
        shell "sh -c \"$(\
                \curl -fsSL \
                \https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh\
              \)\""
              empty
        >>= (\ohmyzshInstallExitStatus ->
          case ohmyzshInstallExitStatus of
            ExitSuccess ->
              shell "sudo usermod --shell $(which zsh) josiah" empty
              >> echo "Oh My Zsh Install Successful"
            ExitFailure _ -> echo "Oh My Zsh Install Failed."
        )
    )

installOhMyZshPlugins :: IO ()
installOhMyZshPlugins = do
  zshCustomPluginsDir <- fmap (flip (</>) ".oh-my-zsh/custom/plugins") home
  zshAutosuggestionsInstalled <- testpath (
    zshCustomPluginsDir </> "zsh-autosuggestions"
    )
  case zshAutosuggestionsInstalled of
    False ->
      shells "git clone https://github.com/zsh-users/zsh-autosuggestions \
            \ ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions"
             empty
    True -> echo "Zsh-Autosuggestions already installed"
  zshSyntaxHighlightingInstalled <- testpath (
    zshCustomPluginsDir </> "zsh-syntax-highlighting"
    )
  case zshSyntaxHighlightingInstalled of
    False ->
      shells "git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
            \ ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
             empty
    True -> echo "Zsh-Syntax-Highlighting already installed."
  nixZshCompletionsInstalled <- testpath (
    zshCustomPluginsDir </> "nix-zsh-completions"
    )
  case nixZshCompletionsInstalled of
    False ->
      shells "git clone git@github.com:spwhitt/nix-zsh-completions.git \
            \ ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/nix-zsh-completions"
             empty
    True -> echo "Nix-Zsh-Completions already installed."

copyDotFilesToHome :: IO ()
copyDotFilesToHome = do
  homedir <- home
  curdir <- pwd
  cp (curdir </> ".bashrc") (homedir </> ".bashrc")
  cp (curdir </> ".zshrc") (homedir </> ".zshrc")
  cp (curdir </> ".tmux.conf") (homedir </> ".tmux.conf")
  cp (curdir </> ".gitconfig") (homedir </> ".gitconfig")
  cptree (curdir </> ".xmonad") (homedir </> ".xmonad")

installPowerline :: IO ()
installPowerline = shellStrictWithErr "pip show powerline-status" empty
  >>= \powerlineInstalled -> case powerlineInstalled of
    (ExitSuccess, stdOutText, stdErrText) ->
      echoText (stdOutText <> stdErrText) >> echo "Powerline already installed."
    (ExitFailure _, stdOutText, stdErrText) ->
      echoText (stdOutText <> stdErrText)
      >> shells "pip install powerline-status" empty
      >> shells "git clone https://github.com/powerline/fonts.git" empty
      >> cd "fonts" >> shells "sh ./install.sh" empty
      >> cd ".." >> rmtree "fonts"

installDocker :: IO ()
installDocker = do
  dockerInstalled <- which "docker"
  case dockerInstalled of
    Just dockerLoc -> echoWhichLocation dockerLoc
                                        "Docker already installed at "
                                        "Docker already installed."
    Nothing -> (
      shells "sudo apt-key add -"
      $ inshell "curl -fsSL https://download.docker.com/linux/ubuntu/gpg" empty
      ) >> shells "sudo apt-add-repository \
               \ \"deb [arch=amd64] https://download.docker.com/linux/ubuntu \
               \ $(lsb_release -cs) \
               \ stable\""
                empty
      >> shells "sudo apt -y update" empty
      >> shells "sudo apt install -y docker-ce docker-ce-cli containerd.io \
                \docker-compose" empty

installKompose :: IO ()
installKompose = which "kompose" >>= \komposeInstalled ->
  case komposeInstalled of
    Just komposeLoc -> echoWhichLocation komposeLoc
                                         "Kompose already installed at "
                                         "Kompose already installed."
    Nothing ->
      shells "curl -L https://github.com/kubernetes/kompose/releases/download/v1.21.0/kompose-linux-amd64 -o kompose"
             empty
      >> chmod executable "./kompose"
      >> shells "sudo mv ./kompose /usr/local/bin/kompose" empty

installKinD :: IO ()
installKinD = which "kind" >>= \kindInstalled ->
  case kindInstalled of
    Just kindLoc -> echoWhichLocation kindLoc
                                      "KinD already installed at "
                                      "KinD already installed."
    Nothing ->
      shells "curl -Lo ./kind https://kind.sigs.k8s.io/dl/v0.8.1/kind-linux-amd64"
             empty
      >> chmod executable "./kind"
      >> shells "sudo mv ./kind /usr/local/bin/kind" empty

installTerraform :: IO ()
installTerraform = which "terraform" >>= \terraformInstalled ->
  case terraformInstalled of
    Just terraformLoc -> echoWhichLocation terraformLoc
                                           "Terraform already installed at "
                                           "Terrafrom already installed."
    Nothing ->
      (shells "sudo apt-key add -"
       $ inshell "curl -fsSL https://apt.releases.hashicorp.com/gpg" empty)
      >> shells "sudo apt-add-repository \
               \ \"deb [arch=amd64] https://apt.releases.hashicorp.com \
               \ $(lsb_release -cs) main\""
                empty
      >> shells "sudo apt -y update" empty
      >> shells "sudo apt install -y terraform" empty

installHaskellIDEEngine :: IO ()
installHaskellIDEEngine = which "hie" >>= \hieInstalled ->
  case hieInstalled of
    Just hieLoc -> echoWhichLocation hieLoc
                                     "Haskell IDE Engine already installed at "
                                     "Haskell IDE Engine already installed."
    Nothing ->
      shells "git clone https://github.com/haskell/haskell-ide-engine \
             \--recurse-submodules"
             empty
      >> cd "haskell-ide-engine" >> shells "stack ./install.hs hie" empty
      >> cd ".." >> rmtree "haskell-ide-engine"

main :: IO ()
main = do
  shell "sudo apt -y update" empty
  aptInstall "curl" "curl" "cURL already installed at " "cURL already installed."
  shells "sudo apt install -y apt-transport-https ca-certificates gnupg-agent \
         \software-properties-common libgtk-3-dev libicu-dev libncurses-dev \
         \libgmp-dev zlib1g-dev libtinfo-dev libc6-dev libffi-dev g++ gcc make \
         \xz-utils gnupg"
         empty
  aptInstall "xclip"
             "xclip"
             "xclip already installed at "
             "xclip already installed."
  aptInstall "snap"
             "snapd"
             "Snappy already installed at "
             "Snappy already installed."
  aptInstall "git"
             "git"
             "Git already installed at "
             "Git already installed."
  aptInstall "vim"
             "vim"
             "Vim already installed at "
             "Vim already installed."
  aptInstall "gvim"
             "vim-gtk3"
             "GVim already installed at "
             "GVim already installed."
  aptInstall "vifm"
             "vifm"
             "Vifm already installed at "
             "Vifm already installed."
  aptInstall "sensors"
             "lm-sensors"
             "Hardware Sensors CLI program already installed at "
             "Hardware Sensors CLI program already installed."
  aptInstall "xdotool"
             "xdotool"
             "xdotool already installed at "
             "xdotool already installed."
  aptInstall "gnome-tweaks"
             "gnome-tweaks"
             "Gnome Tweaks already installed at "
             "Gnome Tweaks already installed."
  installBraveBrowser
  aptInstall "stack"
             "haskell-stack"
             "Haskell Stack tool already installed at "
             "Haskell Stack tool already installed."
  installHaskellIDEEngine
  shell "stack upgrade --binary-only" empty
  aptInstall "tmux"
             "tmux"
             "Tmux already installed at "
             "Tmux already installed."
  aptInstall "zsh" "zsh" "ZSH already installed at " "ZSH already installed."
  installOhMyZsh
  aptInstall "jq" "jq" "jq already installed at " "jq already installed."
  aptInstall "ag"
             "silversearcher-ag"
             "Silver Searcher already installed at "
             "Silver Searcher already installed."
  aptInstall "rg"
             "ripgrep"
             "Ripgrep already installed at "
             "Ripgrep already installed."
  installRustLang
  aptInstall "go"
             "golang"
             "Go already installed at "
             "Go already installed."
  aptInstall "npm" "npm" "NPM already installed at " "NPM already installed."
  aptInstall "python3"
             "python3"
             "Python 3.8 already installed at "
             "Python 3.8 already installed."
  shell "sudo update-alternatives --install \
       \ /usr/bin/python python /usr/bin/python3 1"
        empty
  installOhMyZshPlugins
  installPowerline
  copyDotFilesToHome
  nixInstalled <- which "nix-shell"
  case nixInstalled of
    Just nixShellLoc -> echoWhichLocation nixShellLoc
                                          "nix-shell found at "
                                          "nix-shell found."
    Nothing -> shells "sh" $ inshell "curl -L https://nixos.org/nix/install"
                                     empty
  installDocker
  aptInstall "az"
             "azure-cli"
             "Azure CLI already installed at "
             "Azure CLI already installed."
  aptInstall "kubectl"
             "kubernetes"
             "K8S already installed at "
             "K8S already installed."
  installKompose
  snapInstall "stable"
              "helm"
              "--classic helm"
              "K8S Helm already installed at "
              "K8S Helm already installed."
  installKinD
  installTerraform
  shells "dconf load / < gnome-settings.dconf" empty
  addFlathubRemoteExitCode <-
    shell "flatpak remote-add --if-not-exists flathub \
         \ https://dl.flathub.org/repo/flathub.flatpakrepo"
          empty
  case addFlathubRemoteExitCode of
    ExitSuccess -> 
      flatpakInstall "org.signal.Signal"
      >> flatpakInstall "com.slack.Slack"
      >> flatpakInstall "im.riot.Riot"
      >> flatpakInstall "com.microsoft.Teams"
      >> flatpakInstall "com.spotify.Client"
      >> flatpakInstall "us.zoom.Zoom"
      >> flatpakInstall "com.valvesoftware.Steam"
    ExitFailure exitCode -> die "Could not add the remote 'flathub'."
  snapInstall "stable"
              "emacs"
              "--classic emacs"
              "Emacs already installed at "
              "Emacs already installed."
  snapInstall "edge"
              "thefuck"
              "--classic thefuck"
              "thefuck already installed at "
              "thefuck already installed."
  echo "DONE"
