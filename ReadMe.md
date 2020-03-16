# requirement
## neovim
```bash
npm install -g pynvim
pip install pynvim
```

## zsh
```bash
npm install -g safe-rm
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
ln -s ~/Documents/workspace/dotfiles/zsh/zshrc .zshrc
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

