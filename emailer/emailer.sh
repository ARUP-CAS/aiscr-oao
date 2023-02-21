#!/usr/bin/env bash

# Script reads files from ./emails/ folder and 
# if the todays date fits the date in the file,
# an email is sent to given OAO.
# A BCC is sent to chosen people.

# global variables
TODAY=$(date --iso-8601)
FROM_NAME='AIS CR'
FROM_ADD='info@amapa.cz'
USER=$(< secret.txt sed -n '1p')
PASSWD=$(< secret.txt sed -n '2p')
SMTPSRV=$(< secret.txt sed -n '3p')
TO_BCC1=$(< secret.txt sed -n '6p')
TO_BCC2=$(< secret.txt sed -n '7p')
TO_BCC3=$(< secret.txt sed -n '8p')
TO_BCC4=$(< secret.txt sed -n '9p')
TO_BCC5=$(< secret.txt sed -n '10p')

emails=$(find ./emails/ -type f)

for email in $emails
do

  # variables
  to_ico=$(< $email sed -n '1p')
  to_oao=$(< $email sed -n '2p')
  to_add=$(< $email sed -n '3p')
  send_on=$(< $email sed -n '4p')
  subj=$(< $email sed -n '5p')
  body=$(< $email sed -n '6p')

  if [ "$send_on" == "$TODAY" ]; then
  
  # email text
    cat << EOF > email.eml
From: "${FROM_NAME}" <${FROM_ADD}>
To: ${to_add}
Subject: ${subj}
Content-Type: text/html; charset="utf-8"

${body}
EOF
  
    # send message
    curl -s --ssl $SMTPSRV \
    --mail-from $FROM_ADD \
    --mail-rcpt $to_add \
    --mail-rcpt $TO_BCC1 \
    --mail-rcpt $TO_BCC2 \
    --mail-rcpt $TO_BCC3 \
    --mail-rcpt $TO_BCC4 \
    --mail-rcpt $TO_BCC5 \
    --user "${USER}:${PASSWD}" \
    --upload-file email.eml

    # log entry
    cat << EOF >> emailer.log
"$(date +'%Y-%m-%d %H:%M:%S')", "${to_ico}", "${to_oao}", "${to_add}", "${send_on}", "${subj}"
EOF

    sleep 5

  fi

done

