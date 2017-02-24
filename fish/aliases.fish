alias .. "cd .."
alias ... "cd ../.."
alias - "cd -"

alias la "ls -Gla"

alias ll "ls -ahlF"
alias l "ls -CF"

################################
###  File ShortCut
################################

alias e "emacsclient -t"
alias ec "emacsclient -c"

################################
###  Directory ShortCut
################################
# Directories
alias igdev "cd $IGDEV_SCRIPTS_PATH"
alias archive "cd /usr0/igdevarc/grponly/sw/users/jkowalsk"
alias work "cd /usr0/igdev/grponly/sw/working/jkowalsk"
alias val "cd $VAL_PATH"
alias ace "cd /usr0/igdev/grponly/sw/working/jkowalsk/ace_sw.git"
alias argenta "cd /usr0/igdev/grponly/sw/working/jkowalsk/argenta"
alias elektra "cd /usr0/igdev/grponly/sw/working/jkowalsk/elektra"

alias muse "cd /usr0/igdev/grponly/sw/working/jkowalsk/muse_sw.git"
alias museremote "cd /usr0/igdev/grponly/sw/working/jkowalsk/muse_sw.git/src/linux/museremote/libmuseremote"
alias museradio "cd /usr0/igdev/grponly/sw/working/jkowalsk/muse_sw.git/src/linux/museradio/libmuseradio"
alias musegui "cd /usr0/igdev/grponly/sw/working/jkowalsk/muse_sw.git/src/linux/musegui/libmusegui"
alias museswupdate "cd /usr0/igdev/grponly/sw/working/jkowalsk/muse_sw.git/src/linux/museswupdate/libmuseswupdate"
alias seekif "cd /usr0/igdev/grponly/sw/working/jkowalsk/muse_sw.git/src/linux/seekif/libseekif"
alias dmmif "cd /usr0/igdev/grponly/sw/working/jkowalsk/muse_sw.git/src/linux/dmmif/libdmmif"



alias calapp "cd /usr0/igdev/grponly/sw/working/jkowalsk/ace_sw.git/src/arm/calapp"

# computers
alias godzilla "ssh jkowalsk@godzilla"
#alias isz "ssh pi@ice-station-zebra"
alias frogstar "ssh jkowalsk@frogstar"
alias zardoz "ssh jkowalsk@zardoz"

# other exports
set -gx MUSE_IP 172.16.1.1
alias museboard "ssh root@172.16.1.1"
alias musebaord "museboard"
alias museborad "museboard"


################################
###  Program ShortCut
################################

# git related shortcut
alias gitau "git add -a -u"
alias gitc "git commit"
alias gitpo "git push origin"
alias gitpom "git push origin master"
alias gitlg "git lg"

# turn on coloring on grep
alias fgrep "fgrep --color=auto"
alias egrep "egrep --color=auto"
