#!/usr/bin/python3

# line_seg_intersect1.py

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

import sys
import random

from svgimg.svgimg import Svg
from geom.primitives import LineSeg2D, Point2D
from datastructs.bintree import BinaryTree

def find_intersects(in_lines):
    eq = BinaryTree()
    lines = dict()
    
    for ln in in_lines:
        eq.insert(ln[0])
        eq.insert(ln[1])
        print('tree has size:', eq.size())
        ep = lines.get(ln[0], [])
        ep.append(ln[1])
        lines[ln[0]] = ep

    print(lines)
    print(eq)

def main(args):
    find_intersects(
        [LineSeg2D(Point2D(0,0), Point2D(2,0)),
         LineSeg2D(Point2D(0,-1), Point2D(2,0)),
         LineSeg2D(Point2D(0,1), Point2D(2,1)),
         LineSeg2D(Point2D(-1,-1), Point2D(2,1)),
         LineSeg2D(Point2D(0,0), Point2D(2,-2))])

    # img = Svg()
    # while not bob.empty():
    #     ln = bob.popLowest()
    #     img.add_line(ln)
    #     # print(ln)
    # print(img)
    
if __name__=='__main__':    
    main(sys.argv[1:])
