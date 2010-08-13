#!/bin/bash

function description() { cat <<EOF
This script will help you "download" (=bzr get) the bzr repository,
create a suitable tailor configuration ("create-config"), and
"convert" the local cloned repository into a git repository.

This converter is configured to "download" (=bzr get) the Bazaar
repository at:
  ${srcbzr}
with the project name:
  ${project_name}

When done, use "implode" to remove all temporaries and results.
EOF
}

function usage() { cat <<EOF
  $0 <sub-command>
sub-command:
  download        Step 1
  create-config   Step 2
  convert         Step 3
  implode         Step 4
EOF
echo "" && description
}

# AUTHOR
#  Clifford Caoile <piyo@users.sf.net>
# DATE
#  [2010-08-13 Fri 12:13]
# REQUIREMENTS
#  tailor
#  bzr
#  git
# SUCCESSFULLY TESTED ON Ubuntu 10.04

#
# Preferences
#
srcbzr=${srcbzr:-http://www.robf.de/Hacking/bazaar/ahk-mode}
project_name=${project_name:-ahk-mode}

#
# Code below this line does not need editing, IMHO.
#
subcmd=${1}
shift

case $subcmd in
    download)
        cwdsave=$PWD
        echo ">>>> Hiding .git from bzr (!)" && \
            mv .git .git-hidden
        echo ">>>> Cloning original into original/" && \
            mkdir -p original && cd original && \
            rm -fr ${project_name} && \
            bzr get $srcbzr ${project_name} --no-tree
        echo ">>>> Restoring .git from hidden" && \
            cd "$cwdsave" && mv .git-hidden .git
        ;;
    create-config)
        cwdsave=$PWD
        cat >"$cwdsave/.tailor.config.generated" <<EOF
## this is a comment
[DEFAULT]
verbose = True
patch-name-format = ""

[project]
source = bzr:source
target = git:target
start-revision = INITIAL
root-directory = $cwdsave
state-file = .tailor.state

[bzr:source]
repository =  ${cwdsave}/original/${project_name}
subdir = .bzr

[git:target]
subdir = result/${project_name}
EOF
        ;;
    convert)
        cwdsave=$PWD
        echo ">>>> Hiding .git from bzr (!)" && \
            mv .git .git-hidden
        echo ">>>> Removing temp dirs" && \
            rm -fr .bzr result
        if [ -f .tailor.config.generated -a \
             -d original/${project_name}/.bzr ] ; then
            echo ">>>> Running tailor ..." && \
            tailor -D -c .tailor.config.generated
        fi
        echo ">>>> Restoring .git from hidden" && \
            mv .git-hidden .git
        echo ">>>> Result is in result/. Have fun!"
        ;;
    implode)
        echo ">>>> Removing 'original', temporary, and 'result' directories" && \
        rm -fr original \
            .tailor.config.generated \
            .tailor.state .tailor.state.old project.log \
            .bzr result
        ;;
    *)  usage ;;
esac

# end
