# init zinit

source ${ZSHRC_DIR}/zinit/zinit.zsh

zinit load zsh-users/zsh-autosuggestions

zinit wait lucid for \
    light-mode skywind3000/z.lua \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
        zdharma/fast-syntax-highlighting \
    light-mode zsh-users/zsh-history-substring-search \
    light-mode zdharma/history-search-multi-word \
    atload"zicompinit; zicdreplay" blockf \
        light-mode zsh-users/zsh-completions \
    atload"zicompinit; zicdreplay" blockf \
        light-mode esc/conda-zsh-completion \
    atload"zicompinit; zicdreplay" blockf \
        light-mode greymd/docker-zsh-completion

zinit snippet OMZ::lib/clipboard.zsh
zinit snippet OMZ::lib/completion.zsh
zinit snippet OMZ::lib/git.zsh
zinit snippet OMZ::lib/key-bindings.zsh
zinit snippet OMZ::lib/theme-and-appearance.zsh
zinit snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh
zinit snippet OMZ::plugins/node/node.plugin.zsh
zinit snippet OMZ::plugins/nvm/nvm.plugin.zsh

# zlua conf
alias zc='z -c'      # 严格匹配当前路径的子路径
alias zz='z -i'      # 使用交互式选择模式
alias zf='z -I'      # 使用 fzf 对多个结果进行选择
alias zb='z -b'      # 快速回到父目录

function j() {
    if [[ "$argv[1]" == "-"* ]]; then
        _zlua "$@"
    else
        cd "$@" 2> /dev/null || _zlua "$@"
    fi
}

# starship
# if [[ $ISONSERVER == false &&  ]]; then
check_starship=$(command -v starship >/dev/null 2>&1 || echo $?)

if [[ $check_starship -eq 1 ]]; then
    mkdir -p $HOME/bin
    curl -fsSL https://starship.rs/install.sh | bash -s -- -b $HOME/bin
fi

if [[ $ISONSERVER == false && $check_starship -eq 0 ]]; then
    eval "$(starship init zsh)"
    export STARSHIP_CONFIG=$ZSHRC_DIR/starship
fi

# shell color scheme
BASE16_SHELL=$HOME/.base16-shell
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

