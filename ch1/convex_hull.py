#!/usr/bin/python3

import sys
import random

def dbg_print(st):
    pass
    # print(st)
    # return
    
class Point2D(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, pt2):
        return Point2D(self.x + pt2.x, self.y + pt2.y)

    def __str__(self):
        return '({}, {})'.format(self.x, self.y)

    def __repr__(self):
        return str(self)

    def __lt__(self, b):
        if self.x != b.x: return self.x < b.x
        return self.y < b.y

    def __gt__(self, b):
        if self.x != b.x: return self.x > b.x
        return self.y > b.y
    def __eq__(self, b):
        return self.x == b.x and self.y == b.y

class PointSet(object):
    def __init__(self):
        self.pts = []

    def __init__(self, pts):
        self.pts = pts

    def add(self, pt):
        self.pts.append(pt)
        
    def convexHull(self):
        if len(self.pts) < 2:
            raise Exception('Not enough points to compute convex hull!')

        if len(self.pts) <= 4:
            return sorted(self.pts)

        spts = sorted(self.pts)
        lupper = [spts[0], spts[1]]
        dbg_print('Upper')
        dbg_print('u: {}'.format(lupper))
        for i in range(2,len(self.pts)):
            lupper.append(spts[i])
            dbg_print('u: {}'.format(lupper))
            
            while len(lupper)>2 and (not is_right_of(lupper[-3], lupper[-2],lupper[-1])):
                del lupper[-2]
                dbg_print('u: {}'.format(lupper))

        dbg_print('Lower')
        llower = [spts[-1], spts[-2]]
        dbg_print('l: {}'.format(llower))
        for i in range(len(self.pts)-3, -1, -1):
            dbg_print(i)
            llower.append(spts[i])
            dbg_print('l: {}'.format(llower))
            while len(llower)>2 and (not is_right_of(llower[-3], llower[-2], llower[-1])):
                del llower[-2]
                dbg_print('l: {}'.format(llower))

        del llower[0]
        del llower[-1]

        dbg_print("Upper: {}\nLower: {}".format(lupper, llower))
        return lupper + llower

    def __str__(self):
        return str(self.pts)

    def __repr__(self):
        return str(self.pts)

def is_right_of(lpt1, lpt2, pt):
    # | 1 lpt1.x lpt1.y | 
    # | 1 lpt2.x lpt2.y | < 0 => pt is right of the line between lpt1 and lpt2
    # | 1  pt.x   pt.y  |
    det = (lpt2.x * pt.y + lpt1.x * lpt2.y + lpt1.y * pt.x) - \
        (lpt2.y * pt.x + lpt1.y * lpt2.x + lpt1.x * pt.y)
    # print('|| is {} right of {} {}: {}'.format(pt, lpt1, lpt2, det<0))
    return det < 0

def main(args):
    ten_random_points = [Point2D(random.random()*10, random.random()*10) for x in range(10)]

    print(sorted(ten_random_points))
    print(PointSet(ten_random_points))

if __name__=='__main__':    
    main(sys.argv[1:])
