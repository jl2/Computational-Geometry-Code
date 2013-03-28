#include <iostream>
#include <string>
#include <set>
#include <vector>
#include <queue>
#include <algorithm>
#include <exception>

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

template <typename BTType> class BinaryTree;

template <typename BTType> class BinaryTree {
public:
    BinaryTree() : _site(0), _event(0), _left(0), _right(0), _parent(0) {}

    ~BinaryTree() {
        if (_left) {
            delete _left;
            _left = 0;
        }
        if (_site) {
            delete _site;
            _site = 0;
        }
        if (_right) {
            delete _right;
            _right = 0;
        }
    }

    const BinaryTree *find(const BTType &pt) const {
        if (_site == 0) return 0;

        if (_site != 0 && *_site == pt) {
            return this;
        }

        if (_left && pt < *_site) {
            return _left->find(pt);
        } else if (_right) {
            return _right->find(pt);
        }

        return 0;
    }
    BinaryTree *find_next(const BinaryTree *t) const {
        if (t) {
            if (t->_right) {
                // Go right one, then all the way to the left
                BinaryTree *tmp = t->_right;
                while (tmp->_left) {
                    tmp = tmp->_left;
                }
                return tmp;
            } else if (t->_parent) {
                BinaryTree *tmp = t->_parent;
                if (t == t->_parent->_left) {
                    return tmp;
                }
                while (tmp && tmp->_parent && tmp == tmp->_parent->_right) {
                    tmp = tmp->_parent;
                }
                if (tmp->_parent) {
                    return tmp->_parent;
                }
                return 0;
            }
        }
        return 0;
    }
    BinaryTree *find_prev(const BinaryTree *t) const {
        if (t) {
            if (t->_left) {
                BinaryTree *tmp = t->_left;
                while (tmp && tmp->_right) {
                    tmp = tmp->_right;
                }
                return tmp;
            } else if (t->_parent) {
                BinaryTree *tmp = t->_parent;
                if (tmp == t->_parent->_right) {
                    return tmp;
                }
                // Go up while this is the left child,
                while (tmp && tmp->_parent && tmp == tmp->_parent->_left) {
                    tmp = tmp->_parent;
                }
                if (tmp->_parent) {
                    return tmp->_parent;
                }
                return 0;
                // whil
                // && t->_parent->_left != t) {
                // BinaryTree *tmp = t->_parent->_left;
                // while (tmp && tmp->_right) {
                //     tmp = tmp->_right;
                // }
                // return tmp;
            }
        }
        return 0;
    }

    void remove(const BTType &pt) {
        BinaryTree *tree = const_cast<BinaryTree*>(find(pt));
        if (tree == 0) return;

        if (tree->_right) {
            // Find the next item in the tree and replace tree with it, then delete it
            BinaryTree *tmp = find_next(tree);
            if (tmp) {
                std::swap(tree->_site, tmp->_site);
                tmp->remove(*tmp->_site);
            } else {
                throw std::runtime_error("tree->_right was not null, but no next element found!");
            }
        } else if (tree->_left) {
            BinaryTree *tmp = find_prev(tree);
            if (tmp) {
                std::swap(tree->_site, tmp->_site);
                tmp->remove(*tmp->_site);
            } else {
                throw std::runtime_error("tree->_left was not null, but no previous element found!");
            }
        } else {
            if (tree->_parent) {
                if (tree->_parent->_left == tree) {
                    tree->_parent->_left = 0;
                } else if (tree->_parent->_right == tree) {
                    tree->_parent->_right = 0;
                }
                delete tree;
            } else {
                delete _site;
                _site = 0;
            }
        }
    }

    void setEvent(const BTType &pt, Event *ev) {
        BinaryTree *node = insert(pt);
        if (node && 0==node->_event) {
            node->_event = ev;
        }
    }

    BinaryTree *insert(const BTType &pt) {

        // Tree is empty
        if (_site == 0) {
            _site = new BTType(pt);
            return this;
        }

        if (pt == *_site) return this;

        if (pt<*_site) {
            if (0 == _left) {
                _left = new BinaryTree;
                _left->_parent = this;
            }
            return _left->insert(pt);
        } else {
            if (0==_right) {
                _right = new BinaryTree;
                _right->_parent = this;
            }
            return _right->insert(pt);
        }
    }

    bool isEmpty() {
        return _site == 0 && _left == 0 && _right == 0;
    }

    void inorder(std::ostream &out) const {
        if (_left) {
            _left->inorder(out);
        }
        if (_site) {
            out << (*_site);
        }
        if (_right) {
            _right->inorder(out);
        }
    }
    const BTType *site() const { return _site; }
private:
    BTType *_site;
    Event *_event;
    BinaryTree<BTType> *_left, *_right, *_parent;
};

std::ostream &operator<<(std::ostream &out, const BinaryTree<Point> &e) {
    out << "{";
    e.inorder(out);
    out << "}";
    return out;
}
std::ostream &operator<<(std::ostream &out, const BinaryTree<int> &e) {
    out << "{";
    e.inorder(out);
    out << "}";
    return out;
}


bool event_pointer_compare(Event* a, Event*b) {
    return *a<*b;
}
auto compareFunc = event_pointer_compare;

typedef std::set<Point> point_set;
typedef std::vector<Edge> edge_list;
typedef std::priority_queue<Event*, std::vector<Event*>, decltype(compareFunc)> event_queue;
typedef BinaryTree<Point> PtBTree;

void voronoi(const point_set &pts, edge_list &output) {
    std::cout << "Computing voronoi diagram for points:\n";
    
    event_queue eq(compareFunc);
    output.clear();
    PtBTree stat;

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
            std::cout << "Handling site event at " << thisEvent->point() << "\n";
            if (stat.isEmpty()) {
                std::cout << "Empty status, inserting point.\n";
                stat.insert(thisEvent->point());
            } else {
                PtBTree *tmp = stat.insert(thisEvent->point());
                PtBTree *prev = stat.find_next(tmp);
                if (prev) {
                    std::cout << "Point above: " << *prev->site() << "\n";
                } else {
                    std::cout << "No point above!\n";
                }
                // Status::iterator it = stat.upper_bound(thisEvent->point());
            }
        } else {
            // Handle circle event
            std::cout << "Handling site event at " << thisEvent->point() << "\n";
        }
        delete thisEvent;
    }
}

int main() {
    point_set pts{{7,4},{1,2}, {3,4}, {8,9}, {7,3}};
    edge_list edges;

    {
        std::vector<int> vals{2,1,3};
        BinaryTree<int> omg;
    
        for (const auto &p : vals) {
            omg.insert(p);
        }
    
        std::cout << omg << "\n";
        omg.remove(1);
        std::cout << omg << "\n";
        omg.remove(3);
        std::cout << omg << "\n";
        omg.remove(2);
        std::cout << omg << "\n";
    }
    {
        std::vector<int> vals{1,2,3,4,5};
        BinaryTree<int> omg;
    
        for (const auto &p : vals) {
            omg.insert(p);
        }
    
        std::cout << omg << "{1,2,3,4,5}\n";
        omg.remove(1);
        std::cout << omg << "{2,3,4,5}\n";
        omg.remove(2);
        std::cout << omg << "{3,4,5}\n";
        omg.remove(3);
        std::cout << omg << "{4,5}\n";
        omg.remove(4);
        std::cout << omg << "{5}\n";
        omg.remove(5);
        std::cout << omg << "{} \n";
    }
    {
        std::vector<int> vals{5,3,7,2,4,6,9};
        BinaryTree<int> omg;
    
        for (const auto &p : vals) {
            omg.insert(p);
        }
    
        std::cout << omg << "{2345679}\n";
        omg.remove(5);
        std::cout << omg << "{234679}\n";
        omg.remove(2);
        std::cout << omg << "{34679}\n";
        omg.remove(3);
        std::cout << omg << "{4679}\n";
        omg.remove(6);
        std::cout << omg << "{479}\n";
        omg.remove(9);
        std::cout << omg << "{47} \n";
        omg.remove(4);
        std::cout << omg << "{7} \n";
        omg.remove(7);
        std::cout << omg << "{} \n";
    }
    voronoi(pts, edges);
    return 0;
}
