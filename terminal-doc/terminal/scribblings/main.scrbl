#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     terminal))

@title{Terminal}

@author{Liam Buttitta}

@defmodule[terminal]

A Racket library for manipulating the terminal.

@section{Basic Operations}

@defproc[(terminal-available?)
         boolean?]{
 Returns @racket[#t] if a terminal is available, @racket[#f] otherwise.
}

@defproc[(get-terminal-size)
         (or/c #f (list/c exact-nonnegative-integer? exact-nonnegative-integer?))]{
 Returns the size of the terminal.
}

@defproc[(setup-terminal)
         any]{
 Does several things, in the following order:
 @itemlist[
 @item{Validates that a terminal is available; if none are available, @racket[setup-terminal] raises
   an error.}
 @item{Disables input echoing on the terminal.}
 @item{Disables input buffering and processing.}
 @item{Disables output buffering (but not processing).}
 @item{Enables ANSI escape codes, on platforms where they are disabled by default.}
 ]
}

@defproc[(restore-terminal)
         any]{
 Resets the terminal to the state it was in when @racket[setup-terminal] was
 last called.
}

@defform[(with-terminal body ...+)]{
 Wraps the @racket[body] expressions with calls to @racket[setup-terminal] and to
 @racket[restore-terminal]. The @racket[body] expressions are guaranteed to be run in a properly
 configured terminal, and after the @racket[body] expressions are evaluated, the terminal is
 guaranteed to be reset to the state it was in before the @racket[body] expressions were evaluated.
}

@defform[(with-writes-queued body ...+)]{
 Evaluates the @racket[body] expressions, accumulating all written data. When the @racket[body]
 expressions finish evaluating, all of the accumulated data is written at once.
}

@section{ANSI Escape Codes}

@defproc[(move-cursor-up
          [d exact-positive-integer? 1])
         void?]{
 Moves the cursor upwards @racket[d] rows.
}

@defproc[(move-cursor-down
          [d exact-positive-integer? 1])
         void?]{
 Moves the cursor downwards @racket[d] rows.
}

@defproc[(move-cursor-right
          [d exact-positive-integer? 1])
         void?]{
 Moves the cursor rightwards @racket[d] columns.
}

@defproc[(move-cursor-left
          [d exact-positive-integer? 1])
         void?]{
 Moves the cursor leftwards @racket[d] columns.
}

@defproc[(move-cursor-down-lines
          [d exact-positive-integer? 1])
         void?]{
 Moves the cursor downwards @racket[d] rows, to the beginning of that line.
}

@defproc[(move-cursor-up-lines
          [d exact-positive-integer? 1])
         void?]{
 Moves the cursor upwards @racket[d] rows, to the beginning of that line.
}

@defproc[(move-cursor-to-column
          [y exact-positive-integer? 0])
         void?]{
 Moves the cursor to the column @racket[y] spaces right of the leftmost column.
}

@defproc[(move-cursor-by
          [dx exact-integer?]
          [dy exact-integer?])
         void?]{
 Moves the cursor. @racket[dx] and @racket[dy] are interpreted as displacements relative to the
 cursor's current position. Negative values of @racket[dx] correspond to left, positive values of
 @racket[dx] correspond to right, negative values of @racket[dy] correspond to up, and positive values
 of @racket[dy] correspond to down.
}

@defproc[(move-cursor-to
          [x exact-nonnegative-integer?]
          [y exact-nonnegative-integer?])
         void?]{
 Moves the cursor. @racket[x] and @racket[y] are interpreted as coordinates relative to the top-left
 of the console. Note that, unlike the CUP ANSI escape code, @racket[move-cursor-to] is zero-based.
}

@defproc[(erase-to-end)
         void?]{
 Erases in the display from the cursor to the end of the screen.
}

@defproc[(erase-to-beginning)
         void?]{
 Erases in the display from the cursor to the beginning of the screen.
}

@defproc[(erase-display)
         void?]{
 Erases the entire display.
}

@defproc[(erase-all)
         void?]{
 Erases the entire display, as well as the scrollback buffer.

 The corresponding ANSI escape code is not implemented on all terminals.
}

@defproc[(erase-to-line-end)
         void?]{
 Erases in the display from the cursor to the end of the current line.
}

@defproc[(erase-to-line-beginning)
         void?]{
 Erases in the display from the cursor to the beginning of the current line.
}

@defproc[(erase-line)
         void?]{
 Erases the current line.
}

@defproc[(scroll-up
          [d exact-positive-integer?])
         void?]{
 Scrolls the terminal up @racket[d] lines.
}

@defproc[(scroll-down
          [d exact-positive-integer?])
         void?]{
 Scrolls the terminal down @racket[d] lines.
}

@defproc[(show-cursor)
         void?]{
 Makes the cursor visible.
}

@defproc[(hide-cursor)
         void?]{
 Makes the cursor invisible.
}

@defproc[(reset-attributes)
         void?]{
 Resets all current attributes, so that newly written characters are drawn normally.
}
@defproc[(show-cursor)
         void?]{
 Makes the cursor visible.
}

@defproc[(set-intensity
          [intensity (or/c 'bold 'faint 'normal)])
         void?]{
 Sets the intensity of newly written characters.
}

@defproc[(set-underline
          [underline (or/c 'single 'double 'none)])
         void?]{
 Sets whether and to what extent newly written characters are underlined.
}

@defproc[(set-blink
          [blink (or/c 'slow 'fast 'none)])
         void?]{
 Sets whether and at what speed newly written characters blink.
}

@defproc[(enable-strikethrough)
         void?]{
 Enables strikethrough on newly written characters.
}

@defproc[(disable-strikethrough)
         void?]{
 Disables strikethrough on newly written characters.
}

@defproc[(set-frame
          [frame (or/c 'framed 'encircled 'none)])
         void?]{
 Sets whether and how newly written characters are framed.
}

@defproc[(enable-overline)
         void?]{
 Enables overlining on newly written characters.
}

@defproc[(disable-overline)
         void?]{
 Disables overlining on newly written characters.
}

@defproc[(reset-foreground-color)
         void?]{
 Resets the current foreground color.
}

@defproc[(reset-background-color)
         void?]{
 Resets the current background color.
}

@defproc[(4-bit-color?
          [v any/c])
         boolean?]{
 Returns @racket[#t] if @racket[v] is the name of a 4-bit color, or @racket[#f] otherwise.

 The 16 valid 4-bit colors are @racket['black], @racket['dark-red], @racket['dark-green],
 @racket['dark-yellow], @racket['dark-blue], @racket['dark-magenta], @racket['dark-cyan],
 @racket['gray], @racket['red], @racket['green], @racket['yellow], @racket['blue], @racket['magenta],
 @racket['cyan], and @racket['white].
}

@defproc[(color?
[v any/c])
boolean?]{
        Returns @racket[#t] if @racket[v] is a color, or @racket[#f] otherwise.
}

@defproc[(set-foreground-color/4-bit
          [color 4-bit-color?])
         void?]{
 Sets the current foreground color to @racket[color].
}

@defproc[(set-background-color/4-bit
          [color 4-bit-color?])
         void?]{
 Sets the current background color to @racket[color].
}

@defproc[(set-foreground-color/8-bit
          [color byte?])
         void?]{
 Sets the current foreground color to the 8-bit color corresponding to @racket[color].
}

@defproc[(set-background-color/8-bit
          [color byte?])
         void?]{
 Sets the current background color to the 8-bit color corresponding to @racket[color].
}

@defproc[(set-foreground-color/24-bit
          [r byte?]
          [g byte?]
          [b byte?])
         void?]{
 Sets the current foreground color to the 24-bit color (@racket[r], @racket[g], @racket[b]).
}

@defproc[(set-background-color/24-bit [r byte?]
                                      [g byte?]
                                      [b byte?])
         void?]{
 Sets the current background color to the 24-bit color (@racket[r], @racket[g], @racket[b]).
}

@defproc[(set-foreground-color
          [color color?])
         void?]{
 Sets the current foreground color to @racket[color].
}

@defproc[(set-background-color
          [color color?])
         void?]{
 Sets the current background color to @racket[color].
}

