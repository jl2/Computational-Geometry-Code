#!/usr/bin/python3

# svgimg.py

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

class Svg(object):
    def __init__(self):
        self.pts = []
        self.polys = []
        self.lines = []

    def __str__(self):
        rv = '<?xml version="1.0" standalone="no"?> <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"> <svg width="100%" height="100%" version="1.1" xmlns="http://www.w3.org/2000/svg">'
        for p in self.polys:
            rv += '<polygon points="'
            for pt in p:
                rv += '{},{} '.format(pt[0], pt[1])
            rv += '{},{}'.format(p[0][0], p[0][1])
            rv += '" style="fill:#cccccc; stroke:#000000;stroke-width:1"/>'

        for ln in self.lines:
            rv += '<line x1="{}" y1="{}" x2="{}" y2="{}" style="stroke:rgb(99,99,99);stroke-width:2"/>'.format(ln[0][0], ln[0][1],
                                                                                                               ln[1][0], ln[1][1])

        for pt in self.pts:
            rv += '<circle cx="{}" cy="{}" r="3" stroke="blue" stroke-width="2" fill="green"/>'.format(pt[0], pt[1])

        rv += '</svg>'
        return rv

    def __repr__(self):
        return self.__str__()

    def add_pt(self,pt):
        self.pts.append(pt)

    def add_pts(self, pts):
        for pt in pts:
            self.pts.append(pt)

    def add_poly(self,p):
        self.polys.append(p)

    def add_line(self,l):
        self.lines.append(l)

def main():
    bob = Svg()
    bob.add_pt([25,25])
    bob.add_line([[0,0], [50,50]])
    bob.add_poly([[0,0], [50,50], [50,0]])
    print(bob)

if __name__=='__main__':
    main()
