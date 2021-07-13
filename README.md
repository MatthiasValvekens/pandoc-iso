# pandoc-iso

Haskell Pandoc filter to handle some referencing issues when composing ISO docs in Pandoc

Things it does:

 - Clause numbering & references/links
 - Table numbering & references/links
 - Automatic note numbering


So far there are no DOCX-specific features, I'm still using Leonard's Lua filter (with table handling commented out for now) for those.


Things I plan to sort out, in no particular order:

 - Make it easier to work with normative / informative references from BibTeX.
 - Figure out how to get the internal links to play nice with MS Word editing features (my OOXML chops are a bit lacking, though).
 - Make link styling more neutral


# Build/install

Install Haskell's `stack` toolchain (which is easy to install, but quite beefy) and run `stack install` in the root directory. The first run will take a while (~10 minutes or so), since `stack` has to download and set up a Haskell runtime environment and compile a bunch of libraries, but subsequent runs will be much faster.

This should drop a `pandoc-iso` executable somewhere in a (platform-dependent) canonical location. Said executable is a standard Pandoc JSON filter.

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

## Note auto-numbering

All divs of class `.note` will be automatically rendered with `custom-style="Note"` and prefixed with `NOTE:` if they're the only note in their clause, or `NOTE <number>:` if there are multiple ones.

This only works reliably if the first child of the div is a `Para` or `Plain` element, but I think that's (almost?) always the case.

