Python draft for the final version (apeyroux/osmand@master)

# Usage

    Usage: osmand-mirror [OPTIONS]
    
    Options:
      --destination TEXT  Destination path  [required]
      --debug             Debug
      --proxy TEXT        Proxy HTTP/HTTPS
      --help              Show this message and exit.

    osmand-mirror --destination /var/www/osmand/

# Installation Python

    python setup.py install

# Installation Nix

    nix-env -f . -i

# Dev

## Nix

    nix-shell -p pypi2nix --run "pypi2nix -r requirements.txt -V3 -E libxml2 -E libxslt"
    nix-build

## Docker

    nix-build docker.nix -o docker-image
    cat docker-image | docker load
    docker run -it --rm -v /tmp/www:/www osmand-mirror # mirror to locale /tmp/www
    # or
    docker run -it --rm -v /tmp/www:/www osmand-mirror --proxy http://127.0.0.1:3128

## Installation modules Python

    pip install -r ./requirements.txt
