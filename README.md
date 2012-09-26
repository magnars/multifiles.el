# multifiles.el

An initial attempt at "multifiles" as defined
[here](http://www.reddit.com/r/emacs/comments/10gc9u/can_i_have_multiple_parts_of_buffers_in_one_super/).

## Setup

    (require 'multifiles)

## Usage

Create a new file or buffer with `.multi.` as part of the name,
for instance `some-functions.multi.js`. This is what activates the
multifiles minor mode.

Type in a comment (in js that would be `//`) followed by the multifile
header, which looks like this:

    //--+[ <filename> --- lines 30-48:

When you type the last `:`, the contents of that other file pops up
under the comment. You can now tweak the line numbers to get just what
you want.

Any changes done will be instantly mirrored in the original file.
