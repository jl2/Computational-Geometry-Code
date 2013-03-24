#include <iostream>
#include <string>
#include <set>
#include <vector>
#include <queue>

class Point {
public:
    Point(double x, double y) :_x(x), _y(y){
    }
    bool operator<(const Point& p2) const {
        if (_x == p2._x) {
            return _y < p2._y;
        }
        return _x<p2._x;
    }
    bool operator==(const Point &p2) const {
        return (p2._x == _x) && (p2._y == _y);
    }
    double _x, _y;
};
std::ostream &operator<<(std::ostream &out, const Point &pt) {
    out << "(" << pt._x << ", " << pt._y << ")";
    return out;
}

class Edge {
public:
    Edge(Point p1, Point p2) :_p1(p1), _p2(p2) {}
    bool operator<(const Edge e2) const {
        if (_p1 == e2._p1) return (_p2<e2._p2);
        return (_p1<e2._p1);
    }
    bool operator==(const Edge e2) const {
        return (_p1==e2._p1) && (_p2==e2._p2);
    }
    Point _p1;
    Point _p2;
};
std::ostream &operator<<(std::ostream &out, const Edge &e) {
    out << "[" << e._p1 << " - " << e._p2 << "]";
    return out;
}

typedef enum {site_event, circle_event} event_type;
class Event {
public:
    Event(Point pt, event_type t) : _pt(pt), _valid(true), _et(t) {}
    bool isValid() const { return _valid; }
    void invalidate() { _valid = false; }
    event_type eventType() const { return _et; }
    bool operator<(const Event& e2) const {
        return _pt < e2._pt;
    }
    Point point() const { return _pt; }

private:
    Point _pt;
    bool _valid;
    event_type _et;
};
std::ostream &operator<<(std::ostream &out, const Event &e) {
    static const char* event_names[] = {"site_event", "circle_event"};
    std::cout.setf(std::ios::boolalpha);
    out << "{" << e.point() << " - " << event_names[(int)e.eventType()] << " - " << e.isValid() << "}";
    return out;
}

class STNode {
public:
    STNode(Point site) : _site(site), _circleEvent(0) {}
    void setCircleEvent(Event *ce) { _circleEvent = ce; }
    bool operator<(const STNode &n2) const {
        return _site < n2._site;
    }
    bool operator<(const Point &p) const {
        return p < _site;
    }
    bool operator==(const Point &p) const {
        return p == _site;
    }
    const Point site() { return _site; }

private:
    Point _site;
    Event *_circleEvent;
};

class BinaryTree;
class BinaryTree {
public:
    BinaryTree() : _node(0), _left(0), _right(0), _parent(0) {}
    ~BinaryTree() {
        if (_left) {
            delete _left;
        }
        if (_right) {
            delete _right;
        }
    }
    BinaryTree *find(const Point &p) {
        if (_node != 0) {
            if (*_node == p) {
                return this;
            }
            return 0;
        }
        if (*_node<p) {
            return _right->find(p);
        }
        return _left->find(p);
    }

    BinaryTree *find(STNode *n) {
        return find(n->site());
    }

    void remove(STNode *n) {
        BinaryTree *node = find(n);
        if (node == 0) return;
        

        // It's the parent's left node
        if (n < *(node->parent->_node)) {
            node->parent->_node = node->parent->right->_
        }
    }

    void insert(STNode *n) {
        // Tree is empty
        if (_node == 0 && _left == 0 && _right == 0) {
            _node = n;
            return;
        }
        // Tree has one node then split it so that the values are in the leaves
        if (_node != 0 && _left == 0 && _right == 0) {
            _left = new BinaryTree;
            _right = new BinaryTree;
            _left->_parent = this;
            _right->_parent = this;
            if (*n < *_node) {
                _left->insert(n);
                _right->insert(_node);
            } else {
                _left->insert(_node);
                _right->insert(n);
            }
            return;
        }
        
        if (*n<*_node) {
            _left->insert(_node);
        } else {
            _right->insert(_node);
        }
    }
    bool empty() {
        return _node == 0 && _left == 0 && _right == 0;
    }
    void inorder() {
        if (_node) {
            std::cout << _node->site();
        } else {
            if (_left) {
                _left->inorder();
            }
            if (_right) {
                _right->inorder();
            }
        }
    }
    void findPrevious(const Point &p) {
        
    }
private:
    STNode *_node;
    BinaryTree *_left, *_right, *_parent;
};

typedef std::set<Point> point_set;
typedef std::vector<Edge> edge_list;
typedef std::priority_queue<Event*> event_queue;

void voronoi(const point_set &pts, edge_list &output) {
    std::cout << "Computing voronoi diagram for points:\n";
    
    event_queue eq;
    output.clear();
    BinaryTree stat;

    // step 1
    for (const auto &e : pts) {
        std::cout << e << " ";
        eq.push(new Event(e, site_event));
    }
    std::cout << "\n";

    while (!eq.empty()) {
        Event *thisEvent = eq.top();
        eq.pop();
        std::cout << "Handling event: " << *thisEvent << "\n";
        if (site_event == thisEvent->eventType()) {
            // Handle site event
            std::cout << "Handling circle event at " << thisEvent->point() << "\n";
            if (stat.empty()) {
                std::cout << "Empty status, inserting point.\n";
                stat.insert(new STNode(thisEvent->point()));
            } else {
                std::cout << "OMG!!\n";
                // Status::iterator it = stat.upper_bound(thisEvent->point());
            }
        } else {
            // Handle circle event
            ;
        }
        delete thisEvent;
    }
}

int main() {
    point_set pts{{1,2}, {3,4}, {8,9}, {7,3}};
    edge_list edges;
    BinaryTree omg;
    for (const auto &p : pts) {
        omg.insert(new STNode(p));
    }
    omg.inorder();
    voronoi(pts, edges);

    return 0;
}
