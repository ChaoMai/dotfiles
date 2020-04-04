# dotfiles setup
In a development or online environment,  multiple users may share one account. Settings can be messed up.

Before performing following steps, make sure `$HOME` is set to you own "home" folder.

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
apt update
apt install -y git curl wget

# no installation privilege
# setup $HOME
# setup $PATH
export PATH=/home/maichao/Programs/bin/:$PATH

mkdir -p ~/Documents/workspace
cd ~/Documents/workspace
git clone --recurse-submodules https://github.com/chaomai/dotfiles.git
```

# python

```bash
# macos
brew cask install miniconda

# debian
# https://docs.conda.io/en/latest/miniconda.html#linux-installers
mkdir -p ~/Programs/opt
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
bash Miniconda3-latest-Linux-x86_64.sh
# install into: ~/Programs/opt/miniconda3
# don't run conda init

# cannot use conda at this moment, manually init it
eval "$(~/Programs/opt/miniconda3/bin/conda shell.bash hook)"
conda create --name vim_env_python3.8 python=3.8
conda activate vim_env_python3.8

ln -s ~/Documents/workspace/dotfiles/pythonstartup .pythonstartup
```

# zsh
## install
```bash
# macos
brew install exa lua zsh

# debian
apt install -y exa lua5.3 zsh

# no installation privilege
./configure --prefix=/home/maichao/Programs
make install
```

## configuration
```bash
ln -s ~/Documents/workspace/dotfiles/zsh/zshrc .zshrc
ln -s ~/Documents/workspace/dotfiles/base16-shell .base16-shell

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
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
./configure --prefix=~/Programs
./configure --prefix=/home/maichao/Programs
```

## configuration
```bash
ln -s ~/Documents/workspace/dotfiles/tmux/tmux.conf .tmux.conf
```

# git
```bash
ln -s ~/Documents/workspace/dotfiles/gitconfig .gitconfig
```

# bat
```bash
# macos
brew install bat

# debian
apt install -y bat

# no installation privilege
# https://github.com/sharkdp/bat/releases
# install at ~/Programs/
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
make -j 8 CMAKE_EXTRA_FLAGS="-DCMAKE_INSTALL_PREFIX=/home/maichao/Programs" CMAKE_BUILD_TYPE=Release
make install
```

## configuration
```bash
npm install -g pynvim
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
```

## vimspector

```bash
cd ~/.local/share/nvim/plugged/vimspector
conda activate vim_env_python3.8
./install_gadget.py --enable-c --enable-python --enable-go
conda deactivate
```

# emacs

## build

```bash
-I/usr/local/opt/gnutls/include
-I/usr/local/opt/nettle/include
-I/usr/local/opt/libtasn1/include
-I/usr/local/opt/p11-kit/include

-L/usr/local/opt/lib
-L/usr/local/opt/nettle/lib
-L/usr/local/opt/libtasn1/lib
-L/usr/local/opt/p11-kit/lib


export CC='/usr/bin/clang'
export CXX='/usr/bin/clang++'
export CPPFLAGS="-I/usr/local/opt/libxml2/include/libxml2 -I/usr/local/opt/gmp/include -I/usr/local/opt/mpfr/include -I/usr/local/opt/libmpc/include -I/usr/local/opt/isl/include"
export CFLAGS=${CPPFLAGS}
export LDFLAGS="-L/usr/local/opt/libxml2/lib -L/usr/local/opt/gmp/lib -L/usr/local/opt/mpfr/lib -L/usr/local/opt/libmpc/lib -L/usr/local/opt/isl/lib"
export DYLD_LIBRARY_PATH="/Users/chaomai/Downloads/gcc_build9/lib/"


./configure --with-nativecomp --without-ns --with-json


export CC='/usr/bin/clang'
export CXX='/usr/bin/clang++'
./configure \
--prefix=/Users/chaomai/Downloads/gcc_build9 \
--disable-nls \
--enable-checking=release \
--enable-languages=c,c++,objc,obj-c++,jit \
--enable-host-shared \
--with-gmp=/usr/local/opt/gmp \
--with-mpfr=/usr/local/opt/mpfr \
--with-mpc=/usr/local/opt/libmpc \
--with-isl=/usr/local/opt/isl \
--with-system-zlib \
--disable-multilib \
--disable-bootstrap \
--with-sysroot=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk
```

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

# other

```bash
# tmux

* libevent

​```bash
./configure --prefix=/home/maichao/Programs
​```

* tmux

​```bash
./configure CFLAGS="-I../../Programs/include" LDFLAGS="-L../../Programs/lib" --prefix=/home/maichao/Programs
​```

# vim

​```bash
CFLAGS="-I../../Programs/include"
LDFLAGS="-L../../Programs/lib"
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
            --prefix=/home/maichao/Programs

./configure --with-features=huge \
            --enable-multibyte \
            --enable-rubyinterp=yes \
            --enable-pythoninterp=yes \
            --with-python-config-dir=/usr/local/bin/python2.7-config \
            --enable-luainterp=yes \
            --enable-gui=gtk2 \
            --enable-cscope \
            --prefix=/home/dev/maichao/Programs
​```
```

