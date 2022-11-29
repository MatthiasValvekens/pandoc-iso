# pandoc-iso

![Docker Image Size (tag)](https://img.shields.io/docker/image-size/mfjval/pandoc-iso/latest)


This is a Haskell Pandoc JSON filter to help authoring draft documents in ISO's special .docx template using
lightweight Markdown files as input. The output should be compliant with the 2022 version of said template.


# Feature set

## Supported features

 - Automatically generating the title page from document metadata.
 - Clause numbering & references/links
 - Table numbering & references/links
 - Automatic numbering for notes and examples that is compliant with the ISO directives.
 - Preserve the ISO template's OOXML structure as faithfully as possible.
 - Bibliography & citation management (through `citeproc`)
 - Autopopulating the headers and footers (using OOXML tricks, not Pandoc features)


## Known limitations

 - Currently, the filter does not handle figures since I haven't had any need to myself; feel free to open
an issue if necessary.
 - Unfortunately, due to the limitations of `citeproc`, the normative reference section must be formatted by hand.
   It is still possible to use `citeproc` for citations, though.
 - Table support is currently limited to simple tables (see further down)
 - The ToC is stored as a "dirty" field with a placeholder. Rendering it requires resaving the document in Microsoft Word.
 - Since Pandoc will try to inject some metadata fields (mainly `title`/`author`/`date`) into the document body
   in ways that can't be intercepted by filters (see jgm/pandoc#3109), the `pandoc-iso` filter proactively renames a 
   number of metadata fields. This means that the metadata in the document properties is incomplete/wrong.
 - While regular notes are numbered automatically, notes to entries in the T&D's currently are not.
 - The tight coupling with ISO's template means that `pandoc-iso` is incompatible with quite a few popular filters,
   including but not limited to the `pandoc-xnos` family.

All of these are up for discussion. Feel free to open issues as you see fit.

# Build/install

## Docker

For ease of use, I've published the full conversion tool [as a Docker image](https://hub.docker.com/r/mfjval/pandoc-iso)
labelled `mfjval/pandoc-iso` (amd64 only for the time being). The `docker-compose.yml` file in this repository contains
a sample invocation. Try it:

```bash
touch sample.docx && chmod go+w sample.docx
docker-compose run pandoc-iso
```

Take a look at the resulting `sample.docx` file that was generated from `sample.md`.

Note that the Docker image already includes Pandoc's reference document.

## Standard build

Install Haskell's `stack` toolchain (which is easy to install, but quite beefy) and run `stack install` in the root directory. The first run will take a while (~10 minutes or so), since `stack` has to download and set up a Haskell runtime environment and compile a bunch of libraries, but subsequent runs will be much faster.

The `pandoc-iso` filter does not link against Pandoc itself, it's a standalone executable that only relies on the AST manipulation API in `pandoc-types`.

This should drop a `pandoc-iso` executable somewhere in a (platform-dependent) canonical location. Said executable is a standard Pandoc JSON filter.

Tested/used with Pandoc 2.19.2. For Pandoc versions that support `native_numbering`, make sure to disable that extension in the output settings (that should be the default).


# Hand-crafted reference file

This repository includes a complete OOXML file tree for a reference `.docx` file to pass to Pandoc.
To generate the file, navigate into `data/` and run `./create-reference.sh`. Alternatively, just zip the contents
of `data/reference-template/`.

The OOXML files in that directory are a mix of files extracted from the official ISO reference style sample
(this mainly applies to the style defs and the numbering schemes), together with some carefully hand-crafted OOXML.
The result is a minimal special-purpose document tailored for use with Pandoc.

Note that the structure of the OOXML relies on Pandoc implementation details to work around certain limitations
in the software. It is very likely that those will break if the reference document is resaved using GUI tools---don't do that.
If you don't heed this warning, then more likely than not you will see some errors about unreadable data;
these are recoverable but will probably mess up the headers and footers.

The output is best viewed in MS Word, but LibreOffice will work as well. Some of the more advanced features of the
template will not work or behave slightly differently, but the basics should all be there (this wasn't the case with 
the previous template).


# Usage

## Sample file

Have a look at `sample.md` for a sample document.

## OOXML file structure management

The filter is capable of accurately reproducing the ISO template's OOXML section structure if given proper instructions.

Ensure that your metadata section contains at least the following info (sample):

```yaml
title:  "Document management — Portable Document Format — Integrity protection in encrypted documents in PDF 2.0"
date: 2022-11-30
iso:
  committee: "ISO TC 171/SC 2/WG 8"
  document-ref: "ISO/TS 32004"
  document-year: 2023
  draft-stage: DTS
```

Other than that, keep the following points in mind:

 - Use `\maketitle` to generate the front page & copyright page from document metadata, don't try to build it yourself.
 - Insert `\mainmatter` after the introduction. This will inject a raw OOXML block to switch from Roman to Arabic numbering.
 - The headers and footers will be populated using data from the document metadata. This is done using OOXML fields.

## Clause references

The clause auto-numbering works using (more or less) the same syntax as `pandoc-secnos` (and is incompatible with the latter). By assigning a clause an ID that starts with `sec:`, you can use the `@`-notation to refer to it elsewhere.

Example:

```
# Clause X

## Clause Foo {#sec:foo}

Blablabla

## Clause Bar

Refer to @sec:foo.
```

The `@sec:foo` will be expanded to a hyperlink, with `1.2, "Clause Foo"` as the link text.

## Table references

Putting `{#tbl:my-table}` in a table caption allows you to refer to it later using `@tbl:my-table`. Also here, the prefix `tbl:` is important. The `pandoc-iso` filter will also assign `tbl:my-table` as the table's ID; apparently Pandoc's Markdown reader doesn't do that automatically.

The filter assign the ISO template's table title style to the caption, so it will be auto-numbered.
However, internal references to the table won't use Word's internal auto-numbering! The filter actually tracks table
numbers separately, since the way the numbering scheme is defined in the ISO template makes it difficult to produce
clean "native" internal refs (fortunately ISO doesn't require those). This way of doing things is a bit clumsy, so I
might straighten it out in the future if I find the time to get to the bottom of the issue.

## Note handling & auto-numbering

All divs of class `note` will be automatically rendered with `custom-style="Note"` and prefixed with `NOTE:` if they're the only note in their clause, or `NOTE <number>:` if there are multiple ones.

If the first child of the div is not a `Para` or `Plain` element, a new `Plain` block will be inserted.


Other "note-like" structures that behave similarly:

 - Examples (use `example`);
 - Notes to entry (use `note-to-entry`), albeit without auto-numbering;
 - Editor's notes (use `ed-note`), as an unofficial highlighting device.


## Warning about tables

The way Pandoc renders tables is not compatible with ISO's template. This filter will rectify that, again using raw
OOXML inserts, but only for "simple" tables. That means the following:

 - Only one header row
 - Only one body (i.e. no subheadings)
 - No footer
 - No row/colspans

This set of restrictions was workable for my own use; if you need anything extra, let me know.

# Sample file

After completing the build, try running this command in the repository root:

```bash
pandoc --filter=build/pandoc-iso \
    --citeproc --csl=data/iso690-cite-with-label-en.csl \
    --reference-doc data/reference.docx \
    -f markdown-latex_macros -t docx \
    --no-highlight \
    -o sample.docx sample.md
```

This will compile `sample.md` into `sample.docx` with all bells and whistles enabled.


# Acknowledgement

This work is in many respects indebted to [Leonard Rosenthol's Lua filter](https://github.com/ISOTC171SC2/Pandoc-Common).
One could view this project as a philosophical successor to the latter.
