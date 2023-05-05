# Setup from
# https://timwspence.github.io/blog/posts/2019-08-02-optimized-docker-builds-for-haskell-stack.html
FROM haskell:9.2.7-slim as dependencies

RUN mkdir /opt/build
WORKDIR /opt/build

# Libgmp is linked dynamically, so we have to be careful to consistently version things
RUN apt-get update && apt-get download libgmp10 \
        && mv libgmp*.deb libgmp.deb && rm -rf /var/lib/apt/lists/*

# The idea is that we only invalidate the dependency layer if the lock file changes
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only


FROM haskell:9.2.7-slim as build

RUN apt-get update && apt-get install -y zip && rm -rf /var/lib/apt/lists/*

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build/data

RUN ./create-reference.sh

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin


FROM debian:buster-slim

# Install the same libgmp version
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb


# import a statically linked Pandoc to avoid having to build it ourselves
# (given the size of its dependency tree)
COPY --from=pandoc/minimal:2.19.2-static /pandoc /usr/bin/pandoc

RUN groupadd -g 777 pandoc && useradd -s /bin/bash -u 777 -g 777 -r pandoc

COPY --from=build /opt/build/bin/pandoc-iso /usr/bin/pandoc-iso
RUN mkdir -p /var/pandoc /inputs /outputs

COPY --from=build /opt/build/data/reference.docx /var/pandoc/
COPY ./data/iso690-cite-with-label-en.csl /var/pandoc/biblio.csl
COPY ./docker-scripts/build-draft.sh /usr/bin/

USER pandoc

ENTRYPOINT ["/usr/bin/build-draft.sh"]
