FROM debian:bookworm-slim

COPY --from=pandoc/minimal:2.19.2-static /pandoc /usr/bin/pandoc

RUN groupadd -g 777 pandoc && useradd -s /bin/bash -u 777 -g 777 -r pandoc

COPY ./build/pandoc-iso /usr/bin/pandoc-iso
RUN mkdir -p /var/pandoc /inputs /outputs

COPY ./data/reference.docx /var/pandoc/
COPY ./data/iso690-cite-with-label-en.csl /var/pandoc/biblio.csl
COPY ./docker-scripts/build-draft.sh /usr/bin/

USER pandoc

ENTRYPOINT ["/usr/bin/build-draft.sh"]
