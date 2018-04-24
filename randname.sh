#!/bin/bash
LOGFILE=/home/gepr/ucsf/snap.log

getName() {
  length=$(wc -l booga.csv)
  length=${length% *}
  rand=$(python -c "import random; print random.randint(2,${length});")
  line=$(sed "${rand}q;d" booga.csv)
  sed -i "/${line}/d" booga.csv
  echo "${line}" >> usednames.csv
  echo "Marking ${line} as used." >> ${LOGFILE}
  line=$(echo ${line} | sed 's/ /_/g')
  echo "${line}"
}

name=`getName`
sandboxes="trunks/islj branches/islj-hepn branches/islj-ALTrelease"
DATE=$(date +"%F")
for sb in ${sandboxes}
do
  # check it out
  echo "Checking out ${sb}." >> ${LOGFILE}
  svn co --username gepr_github_export --password "I need 1 account for github exports." https://subversion.assembla.com/svn/bsg-ucsf/${sb}
  # zip it up
  echo "Zipping ${sb} into ${name}-${DATE}.zip." >> ${LOGFILE}
  zip -r9 ${name}-${DATE}.zip ${sb#*/}
done
