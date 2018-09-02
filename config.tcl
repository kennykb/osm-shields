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

# Colours of generic markers for different road classes

set markerColours {
    motorway	#8899ee
    trunk	#ff8888
    primary	#ffcc88
    secondary	#ffff88
    other	#ffffff
}

# Height and width of an average character in the font to be used
# on highway shields

set charHeight 13
set charWidth 7

