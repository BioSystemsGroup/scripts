#!/bin/bash
LOGLEVEL=0
snapdir="/home/gepr/gdrive/TDI:UCSF/TDI:UCSF-shared/analog-snapshots"
LOGFILE="${snapdir}/snap.log"
SRC_DIR=$(dirname ${BASH_SOURCE[0]})

log() {
  if (( LOGLEVEL > 0 )); then echo $1 >> ${LOGFILE}; fi
}

getName() {
  length=$(wc -l ${SRC_DIR}/names.csv)
  length=${length% *}
  rand=$(python -c "import random; print random.randint(2,${length});")
  line=$(sed "${rand}q;d" ${SRC_DIR}/names.csv)
  sed -i "/${line}/d" ${SRC_DIR}/names.csv
  echo "${line}" >> ${SRC_DIR}/usednames.csv
  log "Marking ${line} as used."
  line=$(echo ${line} | sed 's/ /_/g')
  echo "${line}"
}

sandboxes="trunks/islj branches/islj-hepn branches/islj-ALTrelease"
DATE=$(date +"%F")

if ! test -e ${snapdir}; then mkdir -p ${snapdir}; fi
cd ${snapdir}

echo "${DATE} -- Starting snapshot script." >> ${LOGFILE}
for sb in ${sandboxes}
do
  # check it out
  log "Checking out ${sb}."
  svn co -q --username gepr_github_export --password "I need 1 account for github exports." https://subversion.assembla.com/svn/bsg-ucsf/${sb}
  # zip it up
  name=`getName`
  log "Zipping ${sb} into ${name}-${DATE}.zip."
  zip -qr9 ${name}-${DATE}.zip ${sb#*/}
  log "Removing temp ${sb#*/} directory."
  rm -rf ${sb#*/}
  log "Checking for ${sb} sub-directories."
  if ! test -e ${sb}; then mkdir -p ${sb}; fi
  mv ${name}-${DATE}.zip ${sb}/
done
log "Uploading to GDrive."
/usr/local/go/bin/drive push -no-prompt >> ${LOGFILE} 2>&1
echo "${DATE} -- Complete." >> ${LOGFILE}
