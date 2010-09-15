#!/usr/bin/python3

import sys
import random
import xml.dom.minidom

def dbg_print(st):
    pass
    
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

    def to_kml_linestring(self):
        print('<LineString>')
        print('<extrude>1</extrude>')
        print('<tessellate>1</tessellate>')
        print('<altitudeMode>absolute</altitudeMode>')
        print('<coordinates>')
        for pt in self.pts:
            print('{},{},{}'.format(pt.y, pt.x, 2))
        print('</coordinates>')
        print('</LineString>')

    def to_svg_circles(self):
        for pt in self.pts:
            print('<circle cx="{}" cy="{}" r="3" stroke="blue" stroke-width="2" fill="green"/>'.format(pt.x, pt.y))

def is_right_of(lpt1, lpt2, pt):
    # | 1 lpt1.x lpt1.y | 
    # | 1 lpt2.x lpt2.y | < 0 => pt is right of the line between lpt1 and lpt2
    # | 1  pt.x   pt.y  |
    det = (lpt2.x * pt.y + lpt1.x * lpt2.y + lpt1.y * pt.x) - \
        (lpt2.y * pt.x + lpt1.y * lpt2.x + lpt1.x * pt.y)
    # print('|| is {} right of {} {}: {}'.format(pt, lpt1, lpt2, det<0))
    return det < 0

def to_kml_polygon(ptlist):
    print('<Polygon><extrude>1</extrude><altitudeMode>absolute</altitudeMode><outerBoundaryIs><LinearRing>')
    print('<coordinates>')
    for pt in ptlist:
        print('{},{},{}'.format(pt.y, pt.x, 1))
    print('{},{},{}'.format(ptlist[0].y, ptlist[0].x, 1))
    print('</coordinates></LinearRing></outerBoundaryIs></Polygon>')

def main(args):
    pts = []
    
    if len(args)>0:
        with open(args[0]) as fh:
            doc = xml.dom.minidom.parse(fh)
            for trkpt in doc.getElementsByTagName("trkpt"):
                nlat = float(trkpt.getAttribute('lat'))
                nlon = float(trkpt.getAttribute('lon'))
                pts.append(Point2D(nlat, nlon))
                
    else:
        pts = [Point2D(random.randint(1,1000), random.randint(1,1000)) for x in range(100)]

    ps = PointSet(pts)

    print('<?xml version="1.0" encoding="UTF-8"?><kml xmlns="http://www.opengis.net/kml/2.2">')
    print('<Document><name>Paths</name><description>test</description>')
    print('<Style id="yellowLineGreenPoly">')
    print('<LineStyle> <color>7f00ffff</color> <width>4</width> </LineStyle>')
    print('<PolyStyle> <color>7f00ff00</color> </PolyStyle>')
    print('</Style>')

    print('<Style id="#RedLine">')
    print('<LineStyle> <color>7fff0000</color> <width>4</width> </LineStyle>')
    print('</Style>')

    print('<Placemark> <name>testing</name> <description>testing</description><styleUrl>#RedLine</styleUrl>')
    ps.to_kml_linestring()
    print('</Placemark><Placemark> <name>testing</name> <description>testing</description><styleUrl>#yellowLineGreenPoly</styleUrl>')
    to_kml_polygon(ps.convexHull())
    print('</Placemark></Document></kml>')

if __name__=='__main__':    
    main(sys.argv[1:])
