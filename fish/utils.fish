################################
###  Unix Related
################################
function pkill --description "pkill a process interactively"
  ps aux | peco | awk '{ print $2 }' | xargs kill
end

function ppkill --description "kill -9 a process interactively"
  ps aux | peco | awk '{ print $2 }' | xargs kill -KILL
end

function pgrep --description "pgrep a process interactively"
  ps aux | peco | awk '{ print $2 }'
end


#################################
### Network Related
#################################

alias localip="ipconfig getifaddr en1"
alias ips="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ and print $1'"
alias whois="whois -h whois-servers.net"

alias myip="curl -s checkip.dyndns.org | grep -Eo '[0-9\.]+'"
