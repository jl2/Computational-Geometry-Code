#!/usr/bin/env python3

# gpx_hull.py

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

from geom.primitives import Point2D
from convex_hull import PointSet

def to_kml_linestring(ps):
    print('<LineString>')
    print('<extrude>1</extrude>')
    print('<tessellate>1</tessellate>')
    print('<altitudeMode>absolute</altitudeMode>')
    print('<coordinates>')
    for pt in ps:
        print('{},{},{}'.format(pt[1], pt[0], 2))
    print('</coordinates>')
    print('</LineString>')


def to_kml_polygon(ptlist):
    print('<Polygon><extrude>1</extrude><altitudeMode>absolute</altitudeMode><outerBoundaryIs><LinearRing>')
    print('<coordinates>')
    for pt in ptlist:
        print('{},{},{}'.format(pt[1], pt[0], 1))
    print('{},{},{}'.format(ptlist[0][1], ptlist[0][0], 1))
    print('</coordinates></LinearRing></outerBoundaryIs></Polygon>')

def main(args):
    if len(args)==0:
        print("No input file specified")
        exit(1)

    pts = []
    with open(args[0]) as fh:
        doc = xml.dom.minidom.parse(fh)
        for trkpt in doc.getElementsByTagName("trkpt"):
            nlat = float(trkpt.getAttribute('lat'))
            nlon = float(trkpt.getAttribute('lon'))
            pts.append(Point2D(nlat, nlon))

    ps = PointSet(pts)

    print('<?xml version="1.0" encoding="UTF-8"?><kml xmlns="http://www.opengis.net/kml/2.2">')
    print('<Document><name>Paths</name><description>test</description>')
    print('<Style id="yellowLineGreenPoly">')
    print('<LineStyle> <color>7f00ffff</color> <width>4</width> </LineStyle>')
    print('<PolyStyle> <color>7f00ff00</color> </PolyStyle>')
    print('</Style>')

    print('<Style id="RedLine">')
    print('<LineStyle> <color>7f0000ff</color> <width>4</width> </LineStyle>')
    print('<PolyStyle> <color>7fff0000</color> </PolyStyle>')
    print('</Style>')

    print('<Placemark> <name>testing</name> <description>testing</description><styleUrl>#RedLine</styleUrl>')
    to_kml_linestring(ps)
    print('</Placemark><Placemark> <name>testing</name> <description>testing</description><styleUrl>#yellowLineGreenPoly</styleUrl>')
    to_kml_polygon(ps.convexHull())
    print('</Placemark></Document></kml>')

if __name__=='__main__':    
    main(sys.argv[1:])
