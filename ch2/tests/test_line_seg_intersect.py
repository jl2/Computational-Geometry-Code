#/usr/bin/python3

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

from line_seg_intersect1 import *

class TestLineSegmentIntersection(unittest.TestCase):

    def testEventQueueInsert(self):
        bob = EventQueue()
        bob.insert(8)
        self.assertTrue(bob.contains(8))
        bob.insert(4)
        self.assertTrue(bob.contains(4))
        self.assertTrue(bob.contains(8))
        bob.insert(2)
        self.assertTrue(bob.contains(2))
        self.assertTrue(bob.contains(8))

    def testEventQueueMin(self):
        bob = EventQueue()
        self.assertEqual(bob.min(), None)

        bob.insert(3)
        self.assertEqual(bob.min(), 3)

        bob.insert(8)
        self.assertEqual(bob.min(), 3)

        bob.insert(2)
        self.assertEqual(bob.min(), 2)

    def testEventQueueSize(self):
        bob = EventQueue()
        self.assertEqual(bob.size(), 0)
        bob.insert(2)
        self.assertEqual(bob.size(), 1)
        bob.insert(2)
        self.assertEqual(bob.size(), 1)
        bob.insert(3)
        self.assertEqual(bob.size(), 2)
        bob.insert(4)
        self.assertEqual(bob.size(), 3)

    def testEventQueueRemove(self):
        bob = EventQueue()
        self.assertEqual(bob.size(), 0)
        bob.insert(2)
        self.assertEqual(bob.size(), 1)
        bob.insert(3)
        self.assertEqual(bob.size(), 2)
        self.assertTrue(bob.contains(3))
        bob.remove(2)
        self.assertEqual(bob.size(), 1)
        self.assertFalse(bob.contains(2))
        bob.insert(8)
        bob.insert(7)
        bob.insert(9)
        self.assertEqual(bob.size(), 4)
        bob.remove(8)
        self.assertEqual(bob.size(), 3)

    def testEventQueuePopLowest(self):
        bob = EventQueue()
        self.assertEqual(bob.size(), 0)
        bob.insert(2)
        self.assertEqual(bob.popLowest(), 2)
        self.assertEqual(bob.size(), 0)
        bob.insert(0)
        bob.insert(2)
        bob.insert(8)
        bob.insert(-3)
        self.assertEqual(bob.popLowest(), -3)
        self.assertEqual(bob.popLowest(), 0)
        self.assertEqual(bob.popLowest(), 2)
        self.assertEqual(bob.popLowest(), 8)
        self.assertEqual(bob.popLowest(), None)
        self.assertEqual(bob.popLowest(), None)
                
if __name__=='__main__':
    unittest.main()
