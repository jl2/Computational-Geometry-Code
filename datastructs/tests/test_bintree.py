#/usr/bin/python3

# test_bintree.py

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

from bintree import *

class TestBinTree(unittest.TestCase):

    def testNextAndPrev(self):
        bob = BinaryTree()
        bob.insert(7)
        bob.insert(1)
        bob.insert(5)
        bob.insert(4)
        bob.insert(6)
        bob.insert(0)
        bob.insert(8)
        bob.insert(2)
        bob.insert(3)

        self.assertEqual(bob.findNode(3).value, 3)
        self.assertEqual(bob.findNode(3).next().value, 4)
        self.assertEqual(bob.findNode(3).prev().value, 2)
        self.assertEqual(bob.findNode(0).prev(), None)

    def testContains(self):
        bob = BinaryTree()
        bob.insert(7)
        bob.insert(1)
        bob.insert(5)
        bob.insert(4)

        self.assertTrue(bob.contains(5))
        self.assertFalse(bob.contains(2))

    def testRemove(self):
        bob = BinaryTree()
        bob.insert(7)
        bob.insert(1)
        bob.insert(5)
        bob.insert(4)
        bob.insert(6)
        bob.insert(0)
        bob.insert(8)
        bob.insert(2)
        bob.insert(3)

        self.assertTrue(bob.contains(5))
        bob.removeVal(5)
        self.assertFalse(bob.contains(5))
        self.assertTrue(bob.contains(4))
        self.assertTrue(bob.contains(3))

        self.assertTrue(bob.contains(7))
        bob.removeVal(7)
        self.assertFalse(bob.contains(7))
        self.assertTrue(bob.contains(4))
        self.assertTrue(bob.contains(3))
        self.assertTrue(bob.contains(0))
        bob.removeVal(0)
        self.assertFalse(bob.contains(0))
        self.assertTrue(bob.contains(4))
        self.assertTrue(bob.contains(3))
        bob.removeVal(3)
        self.assertFalse(bob.contains(3))

        self.assertTrue(bob.contains(1))
        self.assertTrue(bob.contains(2))
        self.assertTrue(bob.contains(4))
        self.assertTrue(bob.contains(6))
        self.assertTrue(bob.contains(8))

    def testReverseSort(self):
        bob = BinaryTree(int.__gt__)
        bob.insert(7)
        bob.insert(1)
        bob.insert(5)
        bob.insert(4)
        bob.insert(6)
        bob.insert(0)
        bob.insert(8)
        bob.insert(2)
        bob.insert(3)

        bob.removeVal(5)
        self.assertTrue(bob.contains(3))
        bob.removeVal(7)
    
        self.assertTrue(bob.contains(3))
        
        bob.insert(-1)
        bob.insert(9)
        self.assertEqual(bob.findNode(8).next().value, 6)
        self.assertEqual(bob.findNode(8).prev().value, 9)

if __name__=='__main__':
    unittest.main()
