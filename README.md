# Todd's Emacs Config #

I'm primarily a web developer so this config reflects that.  This is a living repository that grows over the years as I grow in my emacs development usage and tooling.  More documentation to come...

## Prereqs

You should have node/npm installed  I install with the `n` node version manager (`brew install n`)

I just use plain gnu emacs

- `brew install emacs --with-cocoa` and then clone this repo into `~/.emacs.d/`

A couple of the packages I install rely on globally installed modules:

- `npm install -g tern` for the ternjs code analysis javascript engine
- `npm install -g eslint` for eslint checking
- `npm install -g prettier` for more linting/formatting
- `npm install -g elm-formatter`
- `npm install -g elm-oracle`
- `brew install aspell` for spell checking
- `brew install the_silver_searcher` for fast code searching
