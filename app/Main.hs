-- The Miniconda3 is not able to be automated due to the way the
-- installation script is implemented.  Please install Miniconda
-- manually before running this script.
-- https://docs.conda.io/en/latest/miniconda.html#linux-installers
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

main :: IO ()
main = do
  shell "sudo apt -y update" empty
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
  -- The following tool always gets installed
  aptInstall "apt-transport-https" "apt-transport-https" "" ""
  aptInstall "curl" "curl" "cURL already installed at " "cURL already installed."
  installBraveBrowser
  aptInstall "stack"
             "haskell-stack"
             "Haskell Stack tool already installed at "
             "Haskell Stack tool already installed."
  shell "stack upgrade --binary-only" empty
  aptInstall "tmux"
             "tmux"
             "Tmux already installed at "
             "Tmux already installed."
  aptInstall "zsh" "zsh" "ZSH already installed at " "ZSH already installed."
  installOhMyZsh
  installRustLang
  aptInstall "python3"
             "python3"
             "Python 3.8 already installed at "
             "Python 3.8 already installed."
  shell "sudo update-alternatives --install \
       \ /usr/bin/python python /usr/bin/python3 1"
        empty
  installOhMyZshPlugins
  copyDotFilesToHome
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
