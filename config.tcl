# Configuration for custom highway shields for OpenStreetMap

# Name of the PostgreSQL database that holds the OpenStreetMap data

set dbname gis

# Prefix that was supplied to 'osm2pgsql' when importing the OpenStreetMap
# data initially

set prefix na_osm

# Directory where the PNG files containing the shield images will be
# installed.

set pngDir /storage/kennykb/Maps/Shields/pngs

# Temporary directory where images under construction may be stored

set tmpDir /tmp/routeGraphics[pid]

# Image sizes required at rendering, in pixels

set sizes {20 24 28 100}
