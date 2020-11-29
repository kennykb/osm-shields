#!/bin/bash

SCRIPT=$(readlink "$0" || echo "$0")
SCRIPTDIR=$(dirname "$SCRIPT")
pushd $SCRIPTDIR > /dev/null ; SCRIPTDIR=$(pwd -L); popd > /dev/null
PROJDIR=$(dirname "$SCRIPTDIR")
pushd $PROJDIR > /dev/null ; PROJDIR=$(pwd -L); popd > /dev/null
echo "Expect to find project files in ${PROJDIR}"

# This file is intended to be run with '. config.sh' to set initial variables
# that are used in multiple scripts.


# Change the following lines to specify what extract from OpenStreetMap
# that you want to load, or launch with the proper environment variables set.

# Name of the PostGIS database to run against
: "${DATABASE:=gis}"

# Prefix to use on the tables in the database
: "${PREFIX:=planet_osm}"

# URL of the .poly file giving the boundaries of the extract

: "${POLYURL:=https://download.geofabrik.de/north-america/us/connecticut.poly}"

# URL of the extract file

: "${EXTURL:=https://download.geofabrik.de/north-america/us/connecticut-latest.osm.pbf}"

# URL of the service that locates the correct minutely 'state.txt'

: "${SEQURL:=https://replicate-sequences.osm.mazdermind.de/}"

# Location of the `trim_osc.py` program

TRIMOSC=/home/kennykb/github/regional-kbk/trim_osc.py

# Size of the 'osm2pgsql' node cache
: "${CACHESIZE:=14000}"

# Number of processes that 'osm2pgsql' may use for import
: "${PROCESSES:=3}"


###############################################################################

# Values derived from the above

POLYFILENAME=$(basename $POLYURL)
EXTFILENAME=$(basename $EXTURL)
