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

# EventQueue is a simple, non-balanced binary search tree
# I probably didn't need to implement this
# I'm not even sure it's implemented "correctly"
class EventQueue(object):
    def __init__(self):
        self.pts = None
    
    def insert(self, ep):
        def innerInsert(node):
            if node is None: return (ep, None, None)
            p, left, right = node
            if ep == p:
                return node
            elif ep < p:
                if left:
                    return (p, innerInsert(left), right)
                else:
                    return (p, (ep, None, None), right)                        
            else:
                if right:
                    return (p, left, innerInsert(right))
                else:
                    return (p, left, (ep, None, None))
        self.pts = innerInsert(self.pts)

    def contains(self, pt):
        def innerContains(node):
            if node is None: return False
            p, left, right = node
            if pt == p:
                return True
            if pt < p:
                return innerContains(left)
            return innerContains(right)
        return innerContains(self.pts)

    def innerMin(self, node):
        if node is None: return None
        p, left, right = node
        if left is None:
            return p
        return self.innerMin(left)

    def min(self):
        return self.innerMin(self.pts)

    def popLowest(self):
        rv = self.innerMin(self.pts)
        if rv is not None: self.remove(rv)
        return rv

    def empty(self):
        return self.pts is None

    def size(self):
        def innerSize(node):
            if node is None: return 0
            p, left, right = node
            return 1 + innerSize(left) + innerSize(right)
        return innerSize(self.pts)

    def remove(self, pt):
        def innerRemove(node, pt):
            if node is None: return None
            p, left, right = node
            if p == pt:
                if left is None and right is None:
                    return None
                elif left is not None and right is None:
                    return left
                elif left is None and right is not None:
                    return right
                else:
                    rm = self.innerMin(right)
                    right = innerRemove(right, rm)
                    return (rm, left, right)
            elif pt < p:
                return (p, innerRemove(left, pt), right)
            else:
                return (p, left, innerRemove(right, pt))
        self.pts = innerRemove(self.pts, pt)

    def __str__(self):
        return str(self.pts)

    def __repr__(self):
        return repr(self.pts)

def main(args):
    bob = EventQueue()
    bob.insert(LineSeg2D(Point2D(0,0), Point2D(2,0)))
    bob.insert(LineSeg2D(Point2D(0,-1), Point2D(2,0)))
    bob.insert(LineSeg2D(Point2D(0,1), Point2D(2,1)))
    bob.insert(LineSeg2D(Point2D(-1,-1), Point2D(2,1)))

    img = Svg()
    while not bob.empty():
        ln = bob.popLowest()
        img.add_line(ln)
        # print(ln)
    print(img)
    
if __name__=='__main__':    
    main(sys.argv[1:])
