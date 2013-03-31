#include <iostream>
#include <string>
#include <set>
#include <list>
#include <algorithm>
#include <stdexcept>

class Point {
public:
    Point(double x, double y) :_x(x), _y(y) {}
    Point(const Point &a) : _x(a._x), _y(a._y) {}

    bool operator<(const Point& p2) const {
        if (_x == p2._x) {
            return _y < p2._y;
        }
        return _x<p2._x;
    }
    bool operator==(const Point &p2) const {
        return (p2._x == _x) && (p2._y == _y);
    }
    const double x() const {
        return _x;
    }
    const double y() const {
        return _y;
    }
private:
    double _x, _y;
};
std::ostream &operator<<(std::ostream &out, const Point &pt) {
    out << "(" << pt.x() << ", " << pt.y() << ")";
    return out;
}

class Triangle {
public:
    Triangle(Point p1, Point p2, Point p3) :_pt1(p1), _pt2(p2),_pt3(p3) {}
    Point _pt1, _pt2, _pt3;
};

typedef std::set<Point> point_set;
typedef std::list<Triangle> tri_list;

std::ostream &operator<<(std::ostream &out, const Triangle &tri) {
    out << "[" << tri._pt1 << ", " << tri._pt2 << ", " << tri._pt3 << "]";
    return out;
}

void delaunay(const point_set &pts, tri_list &output) {
    output.clear();

    std::cout << "Top right is: " << *pts.rbegin() << "\n";
    
    output.emplace_back(Point{0,0}, Point{1,1}, Point{1,0});

}

int main() {
    point_set pts{{7,4},{1,2}, {3,4}, {8,9}, {7,3}};
    tri_list deltris;
    
    std::cout << "Points: ";
    for (const auto &pt : pts) {
        std::cout << pt << " ";
    }
    std::cout << "\n";
    
    delaunay(pts, deltris);
    
    std::cout << "Triangles: ";
    for (const auto &tri : deltris) {
        std::cout << tri << " ";
    }
    std::cout << "\n";

    return 0;
}
