# environment variables
# export MANPATH="/usr/local/man:$MANPATH"
export GOPATH=$HOME/Documents/workspace/go
export PATH=$GOPATH/bin:$PATH

export PYTHONSTARTUP=$HOME/.pythonstartup

export MAVEN_OPTS="-Xmx2g -XX:ReservedCodeCacheSize=512m"

export stack="stack --resolver lts-15.0"

if [[ $ISONSERVER == false ]]; then
    DISABLE_LS_COLORS=true
    alias ls="exa"
else
    alias ls="ls --color"
fi

alias l="$ls -l"
alias ll="$ls -al"

# proxy
if [[ $ISONSERVER == false ]]; then
    alias ssproxy='export http_proxy=http://127.0.0.1:1087 https_proxy=http://127.0.0.1:1087'
    alias noproxy='unset http_proxy https_proxy'
fi

# git
function gi() { curl -L -s https://www.gitignore.io/api/$@ }

if [[ $ISONSERVER == false && $OSTYPE == "Darwin" ]]; then
    alias gittoken="pbcopy < /Users/chaomai/Documents/onedrive/backup_codes_tokens/github_token"
fi

# tmux
function t() { tmux new -A -s $@ }

# docker
alias docker_update_images='docker image ls --format "{{.Repository}}:{{.Tag}}" | xargs -n 1 docker pull'

function toggle_linux_dev() {
    cd /Users/chaomai/Documents/workspace/Docker/LinuxDev

    action=$1
    if [ "$action" = "" ]; then
        ./control.sh toggle
    else
        ./control.sh "$action"
    fi

    cd -
}

# config by os type
if [[ $OSTYPE == "Darwin" ]]; then
    # environment variables
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
        
    # alias
    alias f="open ."
    alias rm="rmtrash"
    alias srm="/bin/rm"

    # util
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
    
    # conda
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
    
    # conda activate python3

    # domob
    source /Users/chaomai/Documents/workspace/domob/utils/shell_utils.sh

elif [[ $OSTYPE == "Linux" ]]; then
    export CC=/usr/local/opt/llvm/bin/clang
    export CXX=/usr/local/opt/llvm/bin/clang++
    
    export SHELL=/bin/zsh
    export EDITOR=/usr/local/bin/vim
    
    export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-13.0.1.jdk/Contents/Home

    # alias
    function rm() {
        local trash_path="$HOME/.Trash"

        if [[ ! -d "${trash_path}" ]]; then
            mkdir -p "${trash_path}"
        fi

        mv $0 ${trash_path}
    }
fi
