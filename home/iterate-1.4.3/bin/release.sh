#!/bin/sh

VERSION=$1

ARCHIVE_NAME=iterate-"$VERSION".tar.gz
ARCHIVE_PATH="/project/iterate/public_html/releases"
ARCHIVE_FULLNAME="$ARCHIVE_PATH/$ARCHIVE_NAME"

if [ -e "$ARCHIVE_FULLNAME" ]; then
  echo "$ARCHIVE_FULLNAME already exists"
  exit -1
fi

echo "Don't forget to tag the darcs repo! Creating release $ARCHIVE_FULLNAME, press enter to continue..."
read
cd /project/iterate/darcs
cp -r iterate iterate-$VERSION
tar --exclude="iterate*/_darcs*" -zvcf "$ARCHIVE_FULLNAME" iterate-$VERSION
rm -rf iterate-$VERSION
cd "$ARCHIVE_PATH"
gpg -b -a "$ARCHIVE_NAME"
ln -fs "$ARCHIVE_NAME" iterate-current.tar.gz
ln -fs "$ARCHIVE_NAME".asc iterate-current.tar.gz.asc

