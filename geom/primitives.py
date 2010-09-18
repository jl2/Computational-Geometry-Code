#!/usr/bin/python3

# primitives.py

# Copyright (c) 2010, Jeremiah LaRocco jeremiah.larocco@gmail.com

# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.

# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

class Point2D(object):
    def __init__(self, x, y):
        self.coords = (x,y)

    def __add__(self, pt2):
        return Point2D(self.coords[0] + pt2.coords[0], self.coords[1] + pt2.coords[1])

    def __str__(self):
        return '({}, {})'.format(self.coords[0], self.coords[1])

    def __repr__(self):
        return str(self)

    def __lt__(self, b):
        if self.coords[0] != b.coords[0]: return self.coords[0] < b.coords[0]
        return self.coords[1] < b.coords[1]

    def __gt__(self, b):
        if self.coords[0] != b.coords[0]: return self.coords[0] > b.coords[0]
        return self.coords[1] > b.coords[1]

    def __eq__(self, b):
        return self.coords[0] == b.coords[0] and self.coords[1] == b.coords[1]

    def __hash__(self):
        return hash(self.coords)

    def __setitem__(self, idx, val):
        self.coords[idx] = val

    def __getitem__(self, idx):
        return self.coords[idx]

class LineSeg2D(object):
    def __init__(self, pt1, pt2):
        if pt1<pt2:
            self.points = (pt1, pt2)
        else:
            self.points = (pt2, pt1)

    def __eq__(self, b):
        return b.points == self.points

    def __lt__(self, b):
        return self.points < b.points

    def __str__(self):
        return ('{{{} -> {}}}'.format(*self.points))

    def __repr__(self):
        return ('{{{} -> {}}}'.format(*self.points))

    def __hash__(self):
        return hash(self.points)

    def __setitem__(self, idx, val):
        self.points[idx] = val

    def __getitem__(self, idx):
        return self.points[idx]
