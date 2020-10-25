FROM haskell:8.8.4

WORKDIR /opt/app

# Set the locale
ENV LANG C.UTF-8

COPY package.yaml package.yaml
COPY stack.yaml stack.yaml

# Update the OS
RUN apt-get update -y

# Install dev deps
RUN stack install --system-ghc brittany ghcide ghcid

# Install the program deps
RUN stack setup && stack build --test --dependencies-only
