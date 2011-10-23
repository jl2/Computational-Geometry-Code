#/usr/bin/env python3

# test_re_parser.py

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

import unittest

import random

from convex_hull import *

class TestConvexHull(unittest.TestCase):

    def testRightOf1(self):
        self.assertTrue(is_right_of(Point2D(1,0),
                                    Point2D(0,0),
                                    Point2D(0,1)))
    def testRightOf2(self):
        self.assertFalse(is_right_of(Point2D(-1,0),
                                    Point2D(0,0),
                                    Point2D(0,1)))
    def testRightOf3(self):
        self.assertFalse(is_right_of(Point2D(0,0.5),
                                    Point2D(0,0),
                                    Point2D(0,1)))
    def testRightOf4(self):
        self.assertTrue(is_right_of(Point2D(0,1),
                                    Point2D(2,0),
                                    Point2D(1,0)))
    def testRightOf5(self):
        self.assertTrue(is_right_of(Point2D(-3,-2),
                                    Point2D(-1,0),
                                    Point2D(1,0)))
    def testRightOf6(self):
        self.assertTrue(is_right_of(Point2D(1,0),
                                    Point2D(0,0),
                                    Point2D(1,1)))
    def testLessThan1(self):
        self.assertTrue(Point2D(0,0) < Point2D(1,0))
        self.assertTrue(Point2D(0,0) < Point2D(0,1))
        self.assertTrue(Point2D(4,4) < Point2D(8,5))
        self.assertFalse(Point2D(1,0) < Point2D(0,0) )
        self.assertFalse(Point2D(0,1) < Point2D(0,0) )
        self.assertFalse(Point2D(8,5) < Point2D(4,4) )

    def testGreaterThan1(self):
        self.assertTrue(Point2D(1,0) > Point2D(0,0) )
        self.assertTrue(Point2D(0,1) > Point2D(0,0) )
        self.assertTrue(Point2D(8,5) > Point2D(4,4) )

        self.assertFalse(Point2D(0,0) > Point2D(1,0))
        self.assertFalse(Point2D(0,0) > Point2D(0,1))
        self.assertFalse(Point2D(4,4) > Point2D(8,5))

    def testEqual(self):
        self.assertTrue(Point2D(0,3) == Point2D(0,3))
        self.assertFalse(Point2D(1,3) == Point2D(0,3))

    def testHull1(self):
        pts = [Point2D(0,0),
               Point2D(0,1),
               Point2D(1,0),
               Point2D(1,1)
               ]
        ps = PointSet(pts)
        self.assertEqual(pts, ps.convexHull())

    def testHull1(self):
        # Square with a single internal point
        pts = [Point2D(0, 0),
               Point2D(0, 2),
               Point2D(2, 0),
               Point2D(2, 2),
               Point2D(1, 1),
               ]
        ps = PointSet(pts)
        self.assertEqual(ps.convexHull(),
                         [Point2D(0,0),
                          Point2D(0,2),
                          Point2D(2,2),
                          Point2D(2,0),
                          ])


    def testHull2(self):
        # Square with a single internal point that cause a lot of problems
        pts = [Point2D(0, 0),
               Point2D(0, 2),
               Point2D(2, 0),
               Point2D(2, 2),
               Point2D(0.1841283425, 0.478515225596),
               ]
        ps = PointSet(pts)

        self.assertEqual([Point2D(0,0),
                          Point2D(0,2),
                          Point2D(2,2),
                          Point2D(2,0),
                          ],
                         ps.convexHull()
                         )

    def testHull3(self):
        # Square with lots of internal points
        pts = [Point2D(0, 0),
               Point2D(0, 2),
               Point2D(2, 0),
               Point2D(2, 2),
               ]
        ps = PointSet(pts)
        for i in range(10):
            ps.add(Point2D(2*random.random(), 2*random.random()))

        self.assertEqual(ps.convexHull(),
                         [Point2D(0,0),
                          Point2D(0,2),
                          Point2D(2,2),
                          Point2D(2,0),
                          ])


if __name__=='__main__':
    unittest.main()
