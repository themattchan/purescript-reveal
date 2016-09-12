# purescript-reveal

This is a small DSL to build slideshows in Purescript, abstracting over thepain of writing raw html.

Currently it is designed towards the Reveal.js presentation structure, but the abstraction should be general enough that the slides can be rendered into any old html slideshow package.

FFI bindings for Reveal.js are not yet implemented, so you cannot yet use the more advanced/interactive features of Reveal.js from Purescript.

## Build the Example

```
npm init-example
```

then run the main function in `src/Reveal.purs` to render the presentation