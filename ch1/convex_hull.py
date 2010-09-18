#!/usr/bin/python3

# convex_hull.py

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
import xml.dom.minidom

from svgimg.svgimg import Svg
from geom.primitives import Point2D

class PointSet(object):
    def __init__(self):
        self.pts = []

    def __init__(self, pts):
        self.pts = [pt for pt in pts]

    def add(self, pt):
        self.pts.append(pt)
        
    def convexHull(self):
        if len(self.pts) < 2:
            raise Exception('Not enough points to compute convex hull!')

        if len(self.pts) <= 4:
            return sorted(self.pts)

        spts = sorted(self.pts)
        lupper = [spts[0], spts[1]]
        for i in range(2,len(self.pts)):
            lupper.append(spts[i])
            while len(lupper)>2 and (not is_right_of(lupper[-3], lupper[-2],lupper[-1])):
                del lupper[-2]

        llower = [spts[-1], spts[-2]]
        for i in range(len(self.pts)-3, -1, -1):
            llower.append(spts[i])
            while len(llower)>2 and (not is_right_of(llower[-3], llower[-2], llower[-1])):
                del llower[-2]

        del llower[0]
        del llower[-1]
        return lupper + llower

    def __getitem__(self, idx):
        return self.pts[idx]

    def __str__(self):
        return str(self.pts)

    def __repr__(self):
        return str(self.pts)

def is_right_of(lpt1, lpt2, pt):
    # | 1 lpt1[0] lpt1[1] | 
    # | 1 lpt2[0] lpt2[1] | < 0 => pt is right of the line between lpt1 and lpt2
    # | 1  pt[0]   pt[1]  |
    det = (lpt2[0] * pt[1] + lpt1[0] * lpt2[1] + lpt1[1] * pt[0]) - \
        (lpt2[1] * pt[0] + lpt1[1] * lpt2[0] + lpt1[0] * pt[1])
    return det < 0

def main(args):
    pts = []
    
    pts = []

    ps = PointSet(Point2D(random.randint(1,1000), random.randint(1,1000)) for x in range(10000))

    tmp = Svg()
    tmp.add_pts(ps)
    tmp.add_poly(ps.convexHull())
    print(tmp)

if __name__=='__main__':    
    main(sys.argv[1:])
