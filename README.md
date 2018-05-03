[![Build Status](https://travis-ci.org/apeyroux/osmand.svg?branch=master)](https://travis-ci.org/apeyroux/osmand)

# Help

Mirror of http://download.osmand.net/indexes.php?xml

``` shell
docker run -it --rm apeyroux/osmand -h
osmand - v1.0

Usage: osmand (-d|--destination ARG) [--proxy-host ARG] [--proxy-port ARG]
              [-f|--filters ARG]
  OsmAnd mirror v1.0

Available options:
  -d,--destination ARG     mirror destination
  --proxy-host ARG         proxy host
  --proxy-port ARG         proxy port
  -f,--filter ARG          filter
  -h,--help                Show this help text`
```

# Create mirror

Example to create a french mirror in /tmp/mirror-osmand

``` shell
docker run -it --rm -v /tmp/mirror-osmand:/mirror apeyroux/osmand -f france \
       -f french \
       -f world \
       -f Guyana \
       -f Spain \
       -f gb_england \
       -f guiana \
       -f italy \
       -f germany \
       -f belgium \
       -f switzerland \
       -f luxembourg \
       -f martinique \
       -f guadeloupe \
       -f oceania \
       -f reunion \
       -f saint-pierre \
       -f mayotte \
       -d /mirror
```
