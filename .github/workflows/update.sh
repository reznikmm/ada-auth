#!/bin/bash
source /etc/profile
export TZ=America/New_York
gprbuild -p -P ~/cvsweb2git/gnat/cvsweb2git.gpr
cd ~/ada-auth/
LAST=`git log -n 1 --format=format:%ad --date=format:%Y.%m.%d.%H.%M.%S`
git config core.autocrlf input
git config user.email "reznikmm@gmail.com"
git config user.name "Max Reznik"
~/cvsweb2git/.obj/cvsweb2git http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/ ./ $LAST
git log -n 1
