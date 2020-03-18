# setup
```bash
git clone --recurse-submodules https://github.com/chaomai/dotfiles.git
```

## zsh
```bash
ln -s ~/Documents/workspace/dotfiles/zsh/zshrc .zshrc
apt install exa

# get into zsh, do initialization
zsh

npm install -g safe-rm
```

## neovim

# requirement
## zsh
```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
npm install -g safe-rm
```

## neovim
```bash
npm install -g pynvim
pip install pynvim
brew install bat
# apt install bat
```

# nerdfont
Some plugins in neovim need glyphs to dislay special icons. Install a [nerd font](https://github.com/ryanoasis/nerd-fonts) or patch your own.

```bash
brew cask install font-firacode-nerd-font
```

**patch**
```bash
brew install fontforge
git clone https://github.com/ryanoasis/nerd-fonts.git

# add following lines to font-patcher
# import site;
# site.addsitedir("/usr/local/lib/python3.7/site-packages")
chmod +x font-patcher
./font-patcher -c --careful PATH_TO_FONT
```

## defx
```bash
pip install Send2Trash
```

## vim-autoformat
```bash
pip install black
pip install jsbeautifier

# cmake-format
# rustfmt
# gofmt
# goimports
```

# common configurations
```bash
ln -s ~/Documents/workspace/dotfiles/base16-shell .base16-shell
ln -s ~/Documents/workspace/dotfiles/gitconfig .gitconfig
ln -s ~/Documents/workspace/dotfiles/pythonstartup .pythonstartup
ln -s ~/Documents/workspace/dotfiles/tmux/tmux.conf .tmux.conf
```

# neovim
```bash
mkdir ~/.config/nvim
cd ~/.config/nvim
ln -s ~/Documents/workspace/dotfiles/vim/vimrc init.vim

# no need to install vim-plug, neovim will load vim-init/autoload/plug.vim automatically.
```

# ccls
## build
### macos
```bash
brew info llvm
cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/local/Cellar/llvm/<Your Version>/lib/cmake
cmake --build Release
```

### debian
```bash
# build from source
sudo apt install clang cmake libclang-dev llvm-dev rapidjson-dev
cmake -H. -BRelease
cmake --build Release
```

