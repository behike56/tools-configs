#(´∀｀)
# Show OS info when opening a new terminal
neofetch

# Set name of the theme to load.
ZSH_THEME="powerlevel9k/powerlevel9k"

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...
export TERM="xterm-256color"
POWERLEVEL9K_MODE='nerdfont-complete'
source  ~/powerlevel9k/powerlevel9k.zsh-theme

######################################################
#misc settings
HIST_STAMPS="mm/dd/yyyy"
DISABLE_UPDATE_PROMPT=true

# Load Zsh tools for syntax highlighting and autosuggestions
HOMEBREW_FOLDER="/usr/local/share"
source "$HOMEBREW_FOLDER/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
source "$HOMEBREW_FOLDER/zsh-autosuggestions/zsh-autosuggestions.zsh"

#battery
prompt_zsh_battery_level() {
  local percentage1=`pmset -g ps  |  sed -n 's/.*[[:blank:]]+*\(.*%\).*/\1/p'`
  local percentage=`echo "${percentage1//\%}"`
  local color='%F{red}'
  local symbol="\uf00d"
  pmset -g ps | grep "discharging" > /dev/null
  if [ $? -eq 0 ]; then
    local charging="false";
  else
    local charging="true";
  fi
  if [ $percentage -le 20 ]
  then symbol='\uf579' ; color='%F{red}' ;
    #10%
  elif [ $percentage -gt 19 ] && [ $percentage -le 30 ]
  then symbol="\uf57a" ; color='%F{red}' ;
    #20%
  elif [ $percentage -gt 29 ] && [ $percentage -le 40 ]
  then symbol="\uf57b" ; color='%F{yellow}' ;
    #35%
  elif [ $percentage -gt 39 ] && [ $percentage -le 50 ]
  then symbol="\uf57c" ; color='%F{yellow}' ;
    #45%
  elif [ $percentage -gt 49 ] && [ $percentage -le 60 ]
  then symbol="\uf57d" ; color='%F{blue}' ;
    #55%
  elif [ $percentage -gt 59 ] && [ $percentage -le 70 ]
  then symbol="\uf57e" ; color='%F{blue}' ;
    #65%
  elif [ $percentage -gt 69 ] && [ $percentage -le 80 ]
  then symbol="\uf57f" ; color='%F{blue}' ;
    #75%
  elif [ $percentage -gt 79 ] && [ $percentage -le 90 ]
  then symbol="\uf580" ; color='%F{blue}' ;
    #85%
  elif [ $percentage -gt 89 ] && [ $percentage -le 99 ]
  then symbol="\uf581" ; color='%F{blue}' ;
    #85%
  elif [ $percentage -gt 98 ]
  then symbol="\uf578" ; color='%F{green}' ;
    #100%
  fi
  if [ $charging = "true" ];
  then color='%F{green}'; if [ $percentage -gt 98 ]; then symbol='\uf584'; fi
  fi
  echo -n "%{$color%}$symbol" ;
}

zsh_wifi_signal(){
    local output=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport -I)
    local airport=$(echo $output | grep 'AirPort' | awk -F': ' '{print $2}')

    if [ "$airport" = "Off" ]; then
        local color='%F{white}'
        echo -n "%{$color%}Wifi Off"
    else
        local ssid=$(echo $output | grep ' SSID' | awk -F': ' '{print $2}')
        local speed=$(echo $output | grep 'lastTxRate' | awk -F': ' '{print $2}')
        local color='%F{white}'

        [[ $speed -gt 100 ]] && color='%F{white}'
        [[ $speed -lt 50 ]] && color='%F{red}'

        echo -n "%{$color%}$speed Mbps \uf1eb%{%f%}" # removed char not in my PowerLine font
    fi
}

######################################################
#Prompt settings
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_PROMPT_ADD_NEWLINE=true

POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX="%F{blue}\u256D\u2500%f"
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%F{blue}\u2570\uf460%f"

POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR='\ue0c6'
POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR='\ue0c7'

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(custom_battery_status_joined time custom_wifi_signal ssh root_indicator dir dir_writable vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(command_execution_time php_version laravel_version status)

######################################################
#LEFT PROMPT
#(custom_battery_status_joined
POWERLEVEL9K_CUSTOM_BATTERY_STATUS="prompt_zsh_battery_level"

#time
#POWERLEVEL9K_TIME_BACKGROUND="blue1"
#POWERLEVEL9K_TIME_FOREGROUND="249"
POWERLEVEL9K_TIME_FORMAT="%D{%I:%M}"

#Wifi-signal
POWERLEVEL9K_CUSTOM_WIFI_SIGNAL="zsh_wifi_signal"
POWERLEVEL9K_CUSTOM_WIFI_SIGNAL_BACKGROUND="black"
POWERLEVEL9K_CUSTOM_WIFI_SIGNAL_FOREGROUND="black"

#dir
POWERLEVEL9K_HOME_ICON='\uf46d'
POWERLEVEL9K_HOME_SUB_ICON='\uf46d'
POWERLEVEL9K_FOLDER_ICON='\uf755'
POWERLEVEL9K_DIR_HOME_BACKGROUND="white"
POWERLEVEL9K_DIR_HOME_FOREGROUND="black"
POWERLEVEL9K_DIR_HOME_SUBFOLDER_FOREGROUND="black"
POWERLEVEL9K_DIR_DEFAULT_FOREGROUND="black"
#POWERLEVEL9K_SHORTEN_DIR_LENGTH=2
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_beginning"
#POWERLEVEL9K_DIR_PATH_SEPARATOR="%F{red}\ue0c0 %F{black}"
POWERLEVEL9K_DIR_HOME_SUBFOLDER_BACKGROUND="white"
POWERLEVEL9K_DIR_HOME_SUBFOLDER_FOREGROUND="black"

POWERLEVEL9K_DIR_PATH_HIGHLIGHT_BACKGROUND="white"
POWERLEVEL9K_DIR_PATH_HIGHLIGHT_FOREGROUND="black"
POWERLEVEL9K_DIR_PATH_SEPARATOR_BACKGROUND="white"
POWERLEVEL9K_DIR_PATH_SEPARATOR_FOREGROUND="black"

#vcs
POWERLEVEL9K_VCS_UNTRACKED_ICON='\u25CF'
POWERLEVEL9K_VCS_UNSTAGED_ICON='\u00b1'
POWERLEVEL9K_VCS_INCOMING_CHANGES_ICON='\u2193'
POWERLEVEL9K_VCS_OUTGOING_CHANGES_ICON='\u2191'
POWERLEVEL9K_VCS_COMMIT_ICON="\uf417"
POWERLEVEL9K_VCS_CLEAN_FOREGROUND='black'
POWERLEVEL9K_VCS_CLEAN_BACKGROUND='green'
POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND='black'
POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND='yellow'
POWERLEVEL9K_VCS_MODIFIED_FOREGROUND='white'
POWERLEVEL9K_VCS_MODIFIED_BACKGROUND='black'

######################################################
#RIGHT PROMPT
#command_execution_time
POWERLEVEL9K_COMMAND_EXECUTION_BACKGROUND="white"
POWERLEVEL9K_COMMAND_EXECUTION_FOREGROUND="black"
POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=0
POWERLEVEL9K_COMMAND_EXECUTION_TIME_BACKGROUND='white'
POWERLEVEL9K_COMMAND_EXECUTION_TIME_FOREGROUND='black'

#php_version (custom)
POWERLEVEL9K_CUSTOM_PHP="echo -n '\ue608"
POWERLEVEL9K_CUSTOM_PHP_FOREGROUND="black"
POWERLEVEL9K_CUSTOM_PHP_BACKGROUND="mediumpurple"

#status
POWERLEVEL9K_STATUS_BACKGROUND="white"
POWERLEVEL9K_STATUS_FOREGROUND="black"
POWERLEVEL9K_STATUS_OK_IN_NON_VERBOSE=true
POWERLEVEL9K_STATUS_VERBOSE=false

######################################################
#commnad customize
#color LS
source $(dirname $(gem which colorls))/tab_complete.sh
alias lc='colorls -lA --sd'

# Coloured man pages using less as pager
man() {
    env \
	      LESS_TERMCAP_mb=$(printf "\e[1;31m") \
	      LESS_TERMCAP_md=$(printf "\e[1;31m") \
	      LESS_TERMCAP_me=$(printf "\e[0m") \
	      LESS_TERMCAP_se=$(printf "\e[0m") \
	      LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
	      LESS_TERMCAP_ue=$(printf "\e[0m") \
	      LESS_TERMCAP_us=$(printf "\e[1;36m") \
	      man "$@"
}

# Aliases for a few useful commands
alias mirrorUpdate="sudo reflector --latest 250 --protocol https --sort rate --save /etc/pacman.d/mirrorlist"
alias yaourt="yaourt --pager --color"
alias pacmanGhost="~/.pacman.sh"
alias shivita="toilet -f mono12 -F rainbow 'andrea' | ponythink -f winona"
alias emacs="emacs -nw"
alias cat="bat"
alias ls="colorls"
alias ip="ip -c"
alias rm="rm -i"
alias x="ranger"
alias c="cmus"
alias h="htop"

#　　 ／￣￣＼
#　／　   _ノ　 　＼
#　|　　　 （ ●）（●）
#.　|　　　　 （__人__）　   zsh + prezto + iTerm2...
#　 |　　　　　｀ ⌒´ﾉ　　   かっこ悪いことはやらないお
#.  |　　　　　　 　 }
#    ヽ　　　　  　 }
#      ヽ　　　　　ノ　　　　　　　　＼
#    　/　　　 く　　＼　　　　　　　 ＼
#    　|　　　　 ＼ 　 ＼ 　 　　　　　　＼
#     |　　　　|ヽ、二⌒)､　 　 　　　　　 ＼
