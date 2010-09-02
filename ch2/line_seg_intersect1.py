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

def main(args):
    print('Nothing yet!')


if __name__=='__main__':    
    main(sys.argv[1:])
