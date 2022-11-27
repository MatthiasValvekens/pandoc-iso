# pandoc-iso

This is a Haskell Pandoc JSON filter to handle some referencing issues when composing ISO docs in Pandoc. I used Haskell because I'm more familiar with it than with Lua, and because it's Pandoc's "native" environment.

Things it does:

 - Clause numbering & references/links
 - Table numbering & references/links
 - Automatic note numbering

Docx-specific things that I merged in from Leonard's Lua filter:

 - Inserting a ToC placeholder
 - Page breaks
 - Metadata rearranging

I didn't adopt the figure handler yet.

Things I plan to sort out, in no particular order:

 - Make it easier to work with normative / informative references from BibTeX. I've been using `citeproc` so far, but it's a bit too simplistic for my tastes.
 - Figure out how to get the internal links to play nice with MS Word editing features (my OOXML chops are a bit lacking, though).
 - Make link styling more neutral


# Build/install

Install Haskell's `stack` toolchain (which is easy to install, but quite beefy) and run `stack install` in the root directory. The first run will take a while (~10 minutes or so), since `stack` has to download and set up a Haskell runtime environment and compile a bunch of libraries, but subsequent runs will be much faster.

The `pandoc-iso` filter does not link against Pandoc itself, it's a standalone executable that only relies on the AST manipulation API in `pandoc-types`.

This should drop a `pandoc-iso` executable somewhere in a (platform-dependent) canonical location. Said executable is a standard Pandoc JSON filter.

Tested/used with Pandoc 2.14.0.2 and Pandoc 2.19.2. For Pandoc versions that support `native_numbering`, make sure to disable that extension in the output settings (that should be the default).

Note: Avoid Pandoc 2.14.1. For those Pandoc versions, disabling `native_numbering` does not disable Pandoc's internal table and figure numbering feature, which interferes with `pandoc-iso`.

# Usage

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

Compatibility note: the syntax is the same as for `pandoc-tablenos`, so you can't use both in the same document.

The filter assign the ISO template's table title style to the caption, so it will be auto-numbered. However, internal references to the table won't use Word's internal auto-numbering! The filter actually tracks table numbers separately, since the way the numbering scheme is defined in the ISO template makes it difficult to produce clean "native" internal refs (fortunately ISO doesn't require those). This way of doing things is a bit clumsy, so I might straighten it out in the future.

## Note auto-numbering

All divs of class `.note` will be automatically rendered with `custom-style="Note"` and prefixed with `NOTE:` if they're the only note in their clause, or `NOTE <number>:` if there are multiple ones.

If the first child of the div is not a `Para` or `Plain` element, a new `Plain` block will be inserted.

## Example auto-numbering

Same as for notes, but replace `.note` with `.example`.
