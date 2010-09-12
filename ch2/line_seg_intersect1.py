#!/usr/bin/python3

import sys
import random

# from collections import namedtuple

def dbg_print(st):
    pass
    # print(st)
    # return
    
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

class LineSeg2D(object):
    def __init__(self, pt1, pt2):
        if pt1<pt2:
            self.points = (pt1, pt2)
        else:
            self.points = (pt2, pt1)

    def __eq__(self, b):
        return b.points == self.points

    def __str__(self):
        return ('{{{} -> {}}}'.format(*self.points))

    def __repr__(self):
        return ('{{{} -> {}}}'.format(*self.points))

    def __hash__(self):
        return hash(self.points)

# QNode = namedtuple('QNode', 'val, left, right')

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
    bob.insert(2)
    print(bob)
    bob.insert(4)
    print(bob)
    bob.insert(1)
    print(bob)
    bob.insert(7)
    bob.insert(9)
    bob.insert(9)
    bob.insert(9)
    print(bob)
    bob.remove(9)
    print(bob)

    bob.remove(9)
    print(bob)
    bob.remove(1)
    print(bob)
    bob.insert(3)
    print(bob)
    bob.remove(4)
    print(bob)

    
if __name__=='__main__':    
    main(sys.argv[1:])
