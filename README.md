# multifiles.el

An initial attempt at "multifiles" as defined
[here](http://www.reddit.com/r/emacs/comments/10gc9u/can_i_have_multiple_parts_of_buffers_in_one_super/).

## Setup

    (require 'multifiles)

## Usage

Open a new buffer, insert the contents of another file like this:

    M-: (mf/insert-mirror "~/.emacs.d/init.el" 30 39)

which would insert the lines 30-39 in the file `~/.emacs.d/init.el`.

You can then repeat that to get multiple files in the buffer. You can also
edit the block, and the changes will be mirrored instantly in the other file.

*Be careful!* Right now the mirror only works one way. Changes in the original files
are not currently mirrored into the multi-file. So any changes might be lost if you're
switching back and forth.

This should be fixable, but this is all I have time for tonight.

## License

Copyright (C) 2011 Magnar Sveen

Author: Magnar Sveen <magnars@gmail.com>
Keywords: multiple files

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
