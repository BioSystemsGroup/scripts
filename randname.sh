#!/bin/bash
snapdir="/home/gepr/gdrive/TDI:UCSF/TDI:UCSF-shared/analog-snapshots"
LOGFILE="${snapdir}/snap.log"
SRC_DIR=$(dirname ${BASH_SOURCE[0]})

getName() {
  length=$(wc -l ${SRC_DIR}/names.csv)
  length=${length% *}
  rand=$(python -c "import random; print random.randint(2,${length});")
  line=$(sed "${rand}q;d" ${SRC_DIR}/names.csv)
  sed -i "/${line}/d" ${SRC_DIR}/names.csv
  echo "${line}" >> ${SRC_DIR}/usednames.csv
  echo "Marking ${line} as used." >> ${LOGFILE}
  line=$(echo ${line} | sed 's/ /_/g')
  echo "${line}"
}

sandboxes="trunks/islj branches/islj-hepn branches/islj-ALTrelease"
DATE=$(date +"%F")

if ! test -e ${snapdir}; then mkdir -p ${snapdir}; fi
cd ${snapdir}

for sb in ${sandboxes}
do
  # check it out
  echo "Checking out ${sb}." >> ${LOGFILE}
  svn co -q --username gepr_github_export --password "I need 1 account for github exports." https://subversion.assembla.com/svn/bsg-ucsf/${sb}
  # zip it up
  name=`getName`
  echo "Zipping ${sb} into ${sb#*/}-${name}-${DATE}.zip." >> ${LOGFILE}
  zip -qr9 ${sb#*/}-${name}-${DATE}.zip ${sb#*/}
  echo "Removing temp ${sb#*/} directory." >> ${LOGFILE}
  rm -rf ${sb#*/}
done
echo "Uploading to GDrive." >> ${LOGFILE}
drive push -no-prompt
