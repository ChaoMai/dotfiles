######################################## environment variables
# export MANPATH="/usr/local/man:$MANPATH"
export GOPATH=$HOME/Documents/workspace/go
export PATH=$GOPATH/bin:$PATH

export PYTHONSTARTUP=$HOME/.pythonstartup

export MAVEN_OPTS="-Xmx2g -XX:ReservedCodeCacheSize=512m"

export stack="stack --resolver lts-15.0"

#################### nvm
export NVM_DIR="$HOME/.nvm"
# this is slow
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

DEFAULT_NODE_VER='default';
while [ -s "$NVM_DIR/alias/$DEFAULT_NODE_VER" ]; do
    DEFAULT_NODE_VER="$(<$NVM_DIR/alias/$DEFAULT_NODE_VER)"
done;

export PATH="$NVM_DIR/versions/node/v${DEFAULT_NODE_VER#v}/bin:$PATH"
alias nvm='unalias nvm; [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use; nvm'

######################################## alias
if [[ $ISONSERVER == false ]]; then
    DISABLE_LS_COLORS=true
    alias ls="exa"
else
    alias ls="ls --color"
fi

alias l="ls -l"
alias ll="ls -al"

######################################## git
function gi() { curl -L -s https://www.gitignore.io/api/$@ }

if [[ $ISONSERVER == false && $OSTYPE == "Darwin" ]]; then
    alias gittoken="pbcopy < /Users/chaomai/Documents/onedrive/backup_codes_tokens/github_token"
fi

######################################## tmux
function t() { tmux new -A -s $@ }

######################################## docker
alias docker_update_images='docker image ls --format "{{.Repository}}:{{.Tag}}" | xargs -n 1 docker pull'

function toggle_linux_dev() {
    cd /Users/chaomai/Documents/workspace/Docker/LinuxDev

    local action=$1
    if [ "$action" = "" ]; then
        ./control.sh toggle
    else
        ./control.sh "$action"
    fi

    cd -
}

######################################## nvim coc
export NVIM_COC_LOG_FILE=/tmp/coc_nvim.log

######################################## config by os type
if [[ $OSTYPE == "Darwin" ]]; then
    ####################  environment variables
    export HOMEBREW_NO_AUTO_UPDATE=true
    export HOMEBREW_GITHUB_API_TOKEN=$(cat $HOME/Documents/onedrive/backup_codes_tokens/brew_api_token)
    # export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles
    # export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles

    export CC=/usr/local/opt/llvm/bin/clang
    export CXX=/usr/local/opt/llvm/bin/clang++

    export SHELL=/bin/zsh
    export EDITOR=/usr/local/bin/vim

    export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-13.0.1.jdk/Contents/Home

    # export PATH=/usr/local/opt/protobuf@2.5/bin:$PATH

    export GNUBIN_COREUTILS=/usr/local/opt/coreutils/libexec/gnubin
    export GNUBIN_FINDUTILS=/usr/local/opt/findutils/libexec/gnubin
    export GNUBIN_INETUTILS=/usr/local/opt/inetutils/libexec/gnubin

    #################### alias
    alias f="open ."
    alias rm="safe-rm"
    alias srm="/bin/rm"

    alias readlink="$GNUBIN_COREUTILS/readlink"

    #################### vim
    # don't use default python
    # don't install vim related package to pollute other's env
    check_nvim=$(command -v nvim >/dev/null 2>&1 || echo $?)

    if [[ $check_nvim -eq 1 ]]; then
        alias vim="PYTHONPATH=/usr/local/Caskroom/miniconda/base/envs/vim_env_python3.8/lib/python3.8/site-packages vim"
        alias v="PYTHONPATH=/usr/local/Caskroom/miniconda/base/envs/vim_env_python3.8/lib/python3.8/site-packages vim"
    else
        alias vim="nvim"
        alias v="nvim"
    fi

    #################### proxy
    alias ssproxy="export http_proxy=http://127.0.0.1:1087 https_proxy=http://127.0.0.1:1087"
    alias unproxy="unset http_proxy https_proxy"

    #################### util
    iconv_to_utf8() {
        local filename=$1
        local dir=$(dirname $filename)
        local file=$(basename $filename)
        local bakfile=${file}_bak
        local tmpfile=${file}_tmp

        cp $dir/$file $dir/$bakfile

        iconv -f GB18030 -t UTF-8 $dir/$file > $dir/$tmpfile
        mv $dir/$tmpfile $dir/$file
    }

    #################### conda
    # >>> conda initialize >>>
    # !! Contents within this block are managed by 'conda init' !!
    # __conda_setup="$('/usr/local/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    # if [ $? -eq 0 ]; then
    # eval "$__conda_setup"
    # else
    if [ -f "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/usr/local/Caskroom/miniconda/base/bin:$PATH"
    fi
    # fi
    # unset __conda_setup
    # <<< conda initialize <<<

elif [[ $OSTYPE == "Linux" ]]; then
    #################### environment variables
    export CC=/usr/bin/clang
    export CXX=/usr/bin/clang++

    export SHELL=/usr/bin/zsh
    export EDITOR=/usr/bin/vim

    export PATH=$HOME/Programs/bin:$PATH

    # export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-13.0.1.jdk/Contents/Home

    #################### alias
    if [[ $ISONSERVER == false ]]; then
        alias ssproxy="export http_proxy=http://192.168.96.1:1087 https_proxy=http://192.168.96.1:1087"
        alias unproxy="unset http_proxy https_proxy"
    fi

    alias rm="safe-rm"
    alias srm="/bin/rm"

    #################### vim
    # don't use default python
    # don't install vim related package to pollute other's env
    if [[ $ISONSERVER == false ]]; then
        check_nvim=$(command -v nvim >/dev/null 2>&1 || echo $?)

        if [[ $check_nvim -eq 1 ]]; then
            alias vim="PYTHONPATH=${HOME}/Programs/opt/miniconda3/envs/vim_env_python3.8/lib/python3.8/site-packages vim"
            alias v="PYTHONPATH=${HOME}/Programs/opt/miniconda3/envs/vim_env_python3.8/lib/python3.8/site-packages vim"
        else
            alias vim="nvim"
            alias v="nvim"
        fi
    fi

    #################### conda
    # >>> conda initialize >>>
    # !! Contents within this block are managed by 'conda init' !!
    # __conda_setup="$('${HOME}/Programs/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    # if [ $? -eq 0 ]; then
    # eval "$__conda_setup"
    # else
    if [ -f "${HOME}/Programs/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "${HOME}/Programs/opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="${HOME}/Programs/opt/miniconda3/bin:$PATH"
    fi
    # fi
    # unset __conda_setup
    # <<< conda initialize <<<
fi

