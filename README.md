# glyphic

Parse ascii drawings like this:

```
           +--------------------|--+
           |                    |  |
     ,-----I-J--------.         |  |
     | id: mainbox    |  JUNK   |  |
     Z Test: box      |         |  |
     | Contains: foo, |  ,--.  ,Q--I---.
     `-----X-Y--------'  |  |  | D: M  |
           | |           `__'  | id: A |
           | |                 `---O--R'
           +-@---------------------+  |
             |                        |
             +------------------------+
        ,-------------.
        |             |
        |      ,------'
        |      |
        `------'
```

and apply a parser to the data inside each proper rectangle.

The result returns a GlyphDrawing which contains a map of retrieved glyphs (gGlyphs) and
gNets, which contains all connections drawn between the rectangles.  Rectangles can have
ports on them, when letters or numbers are embedded on the frame, and those are connected
by groups of lines made of '-', '|', '+' and '@' (+ connects, @ bypasses).

