# dotfiles setup
In a development or online environment,  multiple users may share one account. Settings can be messed up. My dotfiles can be used in non-standard home location. Before performing following steps, make sure `$HOME` is set to you own "home" folder.

```bash
/home/xxx # ok
/home/shared_account/xxx # ok
/Users/xxx # ok
```

# basic
```bash
# macos
brew install git curl

# debian
#
# use sid
# https://mirror.tuna.tsinghua.edu.cn/help/debian/
#
# add following line to /etc/apt/sources.list
# 默认注释了源码镜像以提高 apt update 速度，如有需要可自行取消注释
# deb https://mirrors.tuna.tsinghua.edu.cn/debian/ sid main contrib non-free
# deb-src https://mirrors.tuna.tsinghua.edu.cn/debian/ sid main contrib non-free
apt update
apt install -y git curl wget

# no installation privilege
# setup $HOME
# setup $PATH
export PATH=/home/chaomai/.local/bin/:$PATH

mkdir -p ~/Documents/workspace
cd ~/Documents/workspace
git clone --recurse-submodules https://github.com/chaomai/dotfiles.git
```

# python

```bash
# macos
brew install miniconda

# debian
# https://docs.conda.io/en/latest/miniconda.html#linux-installers
mkdir -p ~/.local/opt
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
bash Miniconda3-latest-Linux-x86_64.sh
# install into: ~/.local/opt/miniconda3
# don't run conda init

# cannot use conda at this moment, manually init it
eval "$(~/.local/opt/miniconda3/bin/conda shell.bash hook)"
conda create --name common_env_python3.9 python=3.9
conda activate common_env_python3.9

ln -s ~/Documents/workspace/dotfiles/pythonstartup .pythonstartup
```

# zsh
## install
```bash
# macos
brew install exa bat lua zsh

# debian
apt install -y exa bat lua5.3 zsh

# no installation privilege
./configure --prefix=/home/chaomai/.local
make install

# https://github.com/ogham/exa/releases
# https://github.com/sharkdp/bat/releases
# install at ~/.local/
```

## configuration
```bash
ln -s ~/Documents/workspace/dotfiles/zsh/zshrc .zshrc
ln -s ~/Documents/workspace/dotfiles/base16-shell .base16-shell

brew install nvm
# cannot use nvm at this moment, manually init it
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

nvm install --lts
npm install -g safe-rm

# get into zsh, do initialization
zsh
```

# tmux
## install
```bash
# macos
brew install tmux

# debian
apt install -y tmux

# no installation privilege
./configure --prefix=~/.local
./configure --prefix=/home/chaomai/.local
```

## configuration
```bash
ln -s ~/Documents/workspace/dotfiles/tmux/tmux.conf .tmux.conf
```

# git
```bash
ln -s ~/Documents/workspace/dotfiles/gitconfig .gitconfig
```

# neovim
## install
**macos**

```bash
# macos
brew install neovim

# debian
apt install -y neovim

# no installation privilege
# https://github.com/neovim/neovim/releases
make -j 8 CMAKE_EXTRA_FLAGS="-DCMAKE_INSTALL_PREFIX=/home/chaomai/.local" CMAKE_BUILD_TYPE=Release
make install
```

## configuration
```bash
npm install -g neovim
pip install pynvim

mkdir -p ~/.config/nvim
cd ~/.config/nvim
ln -s ~/Documents/workspace/dotfiles/vim/vimrc init.vim

# enter vim
:PlugUpdate
```

## nerdfont
Some plugins in neovim need glyphs to dislay special icons. Install a [nerd font](https://github.com/ryanoasis/nerd-fonts) or patch your own.

```bash
# macos
brew cask install font-firacode-nerd-font
```

**patch**

```bash
brew install fontforge
git clone https://github.com/ryanoasis/nerd-fonts.git
fontforge -script font-patcher -c -ext otf --careful PATH_TO_FONT
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

# vim
## install
```bash
# macos
brew install vim

# debian
apt install -y vim-nox

# no installation privilege
# build
```

## configuration

```bash
ln -s ~/Documents/workspace/dotfiles/vim/vimrc .vimrc

# enter vim
:PlugUpdate
```

## ccls

```bash
mkdir -p ~/Documents/workspace/github
git clone https://github.com/MaskRay/ccls.git
```

### build

**macos**

```bash
brew info llvm
cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/local/Cellar/llvm/<Your Version>/lib/cmake
cmake --build Release
```

**debian**

```bash
# build from source
apt install clang cmake libclang-dev llvm-dev rapidjson-dev
cmake -H. -BRelease
cmake --build Release

# when useing llvm-10
cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCLANG_RESOURCE_DIR=/usr/lib/llvm-10/lib/clang/10.0.0
cmake --build Release
```

## vimspector

```bash
cd ~/.local/share/nvim/plugged/vimspector
conda activate common_env_python3.9
./install_gadget.py --enable-c --enable-python --enable-go
conda deactivate
```

# emacs

## build

```bash
# macos
export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:${PATH}"
export LDFLAGS="-L/usr/local/lib/gcc/9"
export CC="clang"

git checkout feature/native-comp
git clean -fdx

./configure \
--disable-silent-rules \
--enable-locallisppath=/usr/local/share/emacs/site-lisp \
--prefix=${HOME}/.local/ \
--with-nativecomp \
--with-ns \
--disable-ns-self-contained

make -j 6 NATIVE_FAST_BOOT=1
make -j 6 NATIVE_FAST_BOOT=1 install
```

**native-comp**
* [gccemacs on OS X](https://gist.github.com/mikroskeem/0a5c909c1880408adf732ceba6d3f9ab/revisions)
* [分享一下 MacOS 下编译 Emacs 28 native-comp 分支](https://emacs-china.org/t/macos-emacs-28-native-comp/12201/)

## install

```bash
# macos
brew tap d12frosted/emacs-plus
brew install emacs-plus

# debian
apt install emacs
```

## configuration

```bash
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
ln -s ~/Documents/workspace/dotfiles/emacs/doom.d .doom.d
~/.emacs.d/bin/doom install
```

# wsl2

wsl2 supports Linux really well.

## gui

```bash
# use a xlient, like VxXsrv
apt install xfce4
startxfce4
```

# build tmux

* libevent

```bash
./configure --prefix=/home/chaomai/.local
```

* tmux

```bash
./configure CFLAGS="-I../../.local/include" LDFLAGS="-L../../.local/lib" --prefix=/home/chaomai/.local
```

# vim

```bash
CFLAGS="-I../../.local/include"
LDFLAGS="-L../../.local/lib"
./configure --with-features=huge \
            --enable-multibyte \
            --enable-rubyinterp=yes \
            --enable-pythoninterp=yes \
            --with-python-config-dir=/usr/local/bin/python2.7-config \
            --enable-python3interp=yes \
            --with-python3-config-dir=/usr/bin/python3.5-config \
            --enable-luainterp=yes \
            --enable-gui=gtk2 \
            --enable-cscope \
            --prefix=/home/chaomai/.local

./configure --with-features=huge \
            --enable-multibyte \
            --enable-rubyinterp=yes \
            --enable-pythoninterp=yes \
            --with-python-config-dir=/usr/local/bin/python2.7-config \
            --enable-luainterp=yes \
            --enable-gui=gtk2 \
            --enable-cscope \
            --prefix=/home/dev/chaomai/.local
```
