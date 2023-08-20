export PATH="$HOME/.cargo/bin:$PATH"

if [ -e /home/josiah/.nix-profile/etc/profile.d/nix.sh ]
then
  . /home/josiah/.nix-profile/etc/profile.d/nix.sh
fi # added by Nix installer

export PATH="${HOME}/.poetry/bin:${PATH}"