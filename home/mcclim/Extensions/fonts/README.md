# McCLIM native TrueType implementation internal notes

The mcclim-fonts/truetype system extends the CLX backend with
antialiased font rendering in 100% Common Lisp (no foreign code),
using the `XRender` extension and the libraries `zpb-ttf` and
`cl-vectors`.

## mcclim-native-ttf

This component contains native implementation of ttf fonts access and
drawing. It is decoupled from `xrender-fonts` component and may be eventually
reused in the future.

Also implementation of `Font listing extension` (see
`Core/clim-basic/ports.lisp`) is provided (except port part, because
it is dependent on the backend which uses `mcclim-native-ttf`).

### Features

* Kerning
* Tracking (letter-spacing)
* Leading (line-spacing)
* Boxes for missing glyphs
* Transformations

### TODO

* Implement fixed-font-width-p for zpb-ttf.
* Make certain left/right bearings and text-bounding-rectangle are
  correct. text-bounding-rectangle and text-size are quite incorrect especially
  when we take multiline into account and align-x/align-y to center/bottom (not
  to mention toward-x/toward-y which are not implemented at all).
* Rethink interface and make it play well with text-style protocol.

### Wish-list

* Subpixel antialiasing. It would be straightforward to generate the
  glyphs by tripling the width as passed to cl-vectors and compressing
  triplets of pixels together ourselves. I'm not certain how to draw
  the result through xrender. I've seen hints on Google that there is
  subpixel AA support in xrender, which isn't obvious from CLX or the 
  spec. Failing that, we could use a 24bpp mask with component-alpha. 
  That might even be how you're supposed to do it. I'm skeptical as to 
  whether this would be accelerated for most people.

* Subpixel positioning. Not hard in principle - render multiple versions
  of each glyph, offset by fractions of a pixel. Horizontal positioning
  is more important than vertical, so 1/4 pixel horizontal resolution
  and 1 pixel vertical resolution should suffice. Given how ugly most
  CLIM apps are, and the lack of WYSIWYG document editors crying out 
  for perfect text spacing in small fonts, we don't really need this.

## fontconfig

One of this component responsibilities is responsible for
estabilishing mapping between text-styles predefined by the
specification and concreete fonts. This is performed by the function
`autoconfigure-fonts` in the following order:

* Predefined paths are scanned for corresponding DejaVu fonts.

* Otherwise we shell out to `fc-match` looking for the fonts proposed
  for our family/face preferences.

* If autoconfiguration fails, user is left on his own (he will be
  asked to configure fonts manually if they are not available).

Each proposed map is tested whenever it may be loaded by
`zpb-ttf:with-font-loader` and only after that considered valid map,
otherwise we reject such configuration.

Additionally device fonts implementation in `xrender-fonts` uses
function `find-fontconfig-font` (the same function we use to shell-out
during the configuration) if it looks for a particular font.

## xrender-fonts

Component is responsible for glueing `mcclim-native-ttf` and the
XRender extension in our CLX backend. Moreover it provides port-wise
part of the implementation for `Font listing extension`
(`port-all-font-families` and `register-all-ttf-fonts`).

We support both `standard-text-style` and `device-font-text-style`. The former
verifies in `text-style-mapping` if the `text-style` is already registered in
the system or if we can load it from the provided
mappings. `device-font-text-style` has a separate method specialization working
with `make-truetype-font` or `fontconfig-font-name` which tries to find
requested font `find-fontconfig-font`.
