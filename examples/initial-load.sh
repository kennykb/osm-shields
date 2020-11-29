#!/bin/bash

# This script loads the initial extract from an OSM mirror into
# the PostGIS database, and creates tables and stored procedures
# needed for shield rendering.

SCRIPT=$(readlink "$0" || echo "$0")
echo "Running $SCRIPT"
SCRIPTDIR=$(dirname "$SCRIPT")
echo "Look for scripts in $SCRIPTDIR"
pushd $SCRIPTDIR > /dev/null ; SCRIPTDIR=$(pwd -L); popd > /dev/null
. "$SCRIPTDIR/config.sh"

# Apply the database table prefix to the Lua script

echo "Preparing scripts to work with $PREFIX tables"
ln -s -f "$PROJDIR/shieldtables.lua" ./
sed "s/@PREFIX@/$PREFIX/g" \
    < "$SCRIPTDIR/compatshields.lua.in" \
    > compatshields.lua

# Get the polygon describing the boundary of the extract data set

if [ -f $POLYFILENAME ]; then
    echo "Not bothering to fetch $POLYFILENAME, it's already there."
else
    echo "Fetching $POLYFILENAME"
    curl "$POLYURL" > "$POLYFILENAME"
fi

# Get the extract to be loaded

echo "Fetching $EXTFILENAME"
curl "$EXTURL" > "$EXTFILENAME"

# Load the extract into the database

osm2pgsql --create \
	  --prefix="$PREFIX" \
	  --slim \
	  --cache=$CACHESIZE \
	  --database=$DATABASE \
	  --number-processes $PROCESSES \
	  --input-reader=pbf \
	  --output=flex \
	  --style=compatshields.lua \
	  "$EXTFILENAME"

# Create the indices on the shield tables

echo "Indexing ${PREFIX}_shieldroute and ${PREFIX}_shieldway"
sed "s/@PREFIX@/$PREFIX/g" \
    < "$PROJDIR/shieldindices.sql.in" \
    | psql -d "$DATABASE" -f -

# Initialize the graphic

echo "Creating the shield graphics. This takes a very long time the"
echo "first time you run it."

tclsh8.6 "$PROJDIR/routeGraphics.tcl" \
	 --init \
	 --prefix $PREFIX

# Set up for database replication

echo "Setting up for database replication with minutely updates"
rm -rf configuration.txt
osmosis --rrii

# Reconstruct the replication state

STAMP="$(osmium fileinfo "$EXTFILENAME" | sed -n '/^ *timestamp=/s///p')"
echo "Reconstructing replication state as of $STAMP"
echo "URL=${SEQURL}?${STAMP}"
curl "${SEQURL}?{$STAMP}" > state.txt
cat state.txt
