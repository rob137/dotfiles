# zsh config layout

`.zshrc-universal` (this repo) is the portable config, symlinked into `~` on each machine:

```sh
ln -s ~/personal/dotfiles/.zshrc-universal ~/.zshrc-universal
```

Each machine then has a local `~/.zshrc` (not in this repo) that sources both:

```sh
# ~/.zshrc
[[ -f ~/.zshrc-universal ]] && source ~/.zshrc-universal
[[ -f ~/.zshrc-local ]] && source ~/.zshrc-local
```

`~/.zshrc-local` (also not in this repo) holds machine-specific config: secrets, local paths, per-machine aliases, etc.
