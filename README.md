# pandoc-iso

Haskell Pandoc filter to handle some referencing issues when composing ISO docs in Pandoc

Things it does:

 - Clause numbering & references/links
 - Table numbering & references/links
 - Automatic note numbering


So far there are no DOCX-specific features, I'm still using Leonard's lua filter (with table handling commented out for now) for those.


Things I plan to sort out, in no particular order:

 - Make it easier to work with normative / informative references from BibTeX.
 - Figure out how to get the internal links to play nice with MS Word editing features (my OOXML chops are a bit lacking, though).
 - Make link styling more neutral


# Build/install

```
stack init
stack build
stack install
```

This should drop a `pandoc-iso` executable somewhere in a (platform-dependent) canonical location. Said executable is a standard Pandoc JSON filter.
