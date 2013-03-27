#include <iostream>
#include <string>
#include <set>
#include <vector>
#include <queue>
#include <algorithm>

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

class TreeNode;
class TreeNode {
public:
    TreeNode() : _site(0), _event(0), _left(0), _right(0), _parent(0) {}

    ~TreeNode() {
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

    const TreeNode *find(const Point &pt) const {
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
    TreeNode *find_next(const TreeNode *t) const {
        if (t) {
            if (t->_right) {
                TreeNode *tmp = t->_right;
                while (tmp->_left) {
                    tmp = tmp->_left;
                }
                return tmp;
            } else if (t->_parent && t->_parent->_right != t) {
                TreeNode *tmp = t->_parent->_right;
                while (tmp->_left) {
                    tmp = tmp->_left;
                }
                return tmp;
            }
        }
        return 0;
    }
    TreeNode *find_prev(const TreeNode *t) const {
        if (t) {
            if (t->_left) {
                TreeNode *tmp = t->_left;
                while (tmp->_right) {
                    tmp = tmp->_right;
                }
                return tmp;
            } else if (t->_parent && t->_parent->_left != t) {
                TreeNode *tmp = t->_parent->_left;
                while (tmp->_right) {
                    tmp = tmp->_right;
                }
                return tmp;
            }
        }
        return 0;
    }

    // void remove(const Point &pt) {
    //     TreeNode *tree = const_cast<TreeNode*>(find(pt));
    //     if (tree == 0) return;
        
    //     if (tree == this) {
    //         // Deleting root!
    //         if (_right) {
    //             TreeNode *tmp = find_next(this);
    //             if (tmp) {
    //                 std::swap(_site, tmp->_site);
    //                 std::swap(_left, tmp->_left);
    //                 std::swap(_right, tmp->_right);
    //                 std::swap(_parent, tmp->_parent);
    //                 tmp->_left = tmp->_right = 0;
    //                 delete tmp;
    //             }
    //         } else if (_left) {
    //             TreeNode *tmp = find_prev(this);
    //             if (tmp) {
    //                 std::swap(_site, tmp->_site);
    //                 std::swap(_left, tmp->_left);
    //                 std::swap(_right, tmp->_right);
    //                 std::swap(_parent, tmp->_parent);
    //                 tmp->_left = tmp->_right = 0;
    //                 delete tmp;
    //             }
    //         } else {
    //             delete _site;
    //             _site = 0;
    //         }
    //         return;
    //     }

    //     if (tree->_right) {
    //         // Two children

    //         // Find the next value and put it here
    //         TreeNode *tmp = find_next(tree);
    //         std::swap(tree->_site, tmp->_site);
    //         tmp->remove(pt);
    //     } else if (tree->_left) {
    //         if (tree->_parent) {
    //             if (tree->_parent->_left == tree) {
    //                 // Deleting parent's left node
    //                 // Make paren'ts left node point to tree's left
    //                 tree->_parent->_left = tree->_left;
    //                 if (tree->_left) {
    //                     tree->_left->_parent = tree->_parent;
    //                 }
    //             } else if (tree->_parent->_right == tree) {
    //                 // Deleting parent's right node
    //                 // Make parent's right point to tree's left
    //                 tree->_parent->_right = tree->_left;
    //                 if (tree->_left) {
    //                     tree->_left->_parent = tree->_parent;
    //                 }
    //             }
    //             tree->_left = 0;
    //             tree->_right = 0;
    //             delete tree;
    //         } else {
    //             TreeNode *tmp = tree->_left;
    //             tree->_site = tree->_left->_site;
    //             tree->_left = tree->_left->_left;
    //             tree->_right = tree->_left->_right;
    //             delete tmp;
    //         }
    //     } else if (tree->_right) {
    //         // Right tree only
    //         if (tree->_parent) {
    //             if (tree->_parent->_left == tree) {
    //                 // Deleting parent's left node
    //                 // Make parent's left node point to tree's right
    //                 tree->_parent->_left = tree->_right;
    //                 if (tree->_right) {
    //                     tree->_right->_parent = tree->_parent;
    //                 }
    //             } else if (tree->_parent->_right == tree) {
    //                 // Deleting parent's right node
    //                 // Make parent's right node point to tree's right
    //                 tree->_parent->_right = tree->_right;
    //                 if (tree->_right) {
    //                     tree->_right->_parent = tree->_parent;
    //                 }
    //             }
    //             tree->_left = 0;
    //             tree->_right = 0;
    //             delete tree;
    //         } else {
    //             TreeNode *tmp = tree->_right;
    //             tree->_site = tree->_right->_site;
    //             tree->_left = tree->_right->_left;
    //             tree->_right = tree->_right->_right;
    //             delete tmp;
    //         }
    //     } else {
    //         // No children - easy case
    //         delete tree;
    //         // if (tree->_site) {
                
    //         // }
    //     }
    // }

    void remove(const Point &pt) {
        TreeNode *tree = const_cast<TreeNode*>(find(pt));
        if (tree == 0) return;
        
        if (tree->_right) {
            TreeNode *tmp = find_next(tree);
            if (tmp) {
                std::swap(tree->_site, tmp->_site);
                std::swap(tree->_left, tmp->_left);
                std::swap(tree->_right, tmp->_right);
                std::swap(tree->_parent, tmp->_parent);
                tmp->_left = tmp->_right = 0;
                delete tmp;
            }
        } else if (tree->_left) {
            TreeNode *tmp = find_prev(tree);
            if (tmp) {
                std::swap(tree->_site, tmp->_site);
                std::swap(tree->_left, tmp->_left);
                std::swap(tree->_right, tmp->_right);
                std::swap(tree->_parent, tmp->_parent);
                tmp->_left = tmp->_right = 0;
                delete tmp;
            }
        } else {
            delete _site;
            _site = 0;
        }
    }

    //     if (tree->_right) {
    //         // Two children

    //         // Find the next value and put it here
    //         TreeNode *tmp = find_next(tree);
    //         std::swap(tree->_site, tmp->_site);
    //         tmp->remove(pt);
    //     } else if (tree->_left) {
    //         if (tree->_parent) {
    //             if (tree->_parent->_left == tree) {
    //                 // Deleting parent's left node
    //                 // Make paren'ts left node point to tree's left
    //                 tree->_parent->_left = tree->_left;
    //                 if (tree->_left) {
    //                     tree->_left->_parent = tree->_parent;
    //                 }
    //             } else if (tree->_parent->_right == tree) {
    //                 // Deleting parent's right node
    //                 // Make parent's right point to tree's left
    //                 tree->_parent->_right = tree->_left;
    //                 if (tree->_left) {
    //                     tree->_left->_parent = tree->_parent;
    //                 }
    //             }
    //             tree->_left = 0;
    //             tree->_right = 0;
    //             delete tree;
    //         } else {
    //             TreeNode *tmp = tree->_left;
    //             tree->_site = tree->_left->_site;
    //             tree->_left = tree->_left->_left;
    //             tree->_right = tree->_left->_right;
    //             delete tmp;
    //         }
    //     } else if (tree->_right) {
    //         // Right tree only
    //         if (tree->_parent) {
    //             if (tree->_parent->_left == tree) {
    //                 // Deleting parent's left node
    //                 // Make parent's left node point to tree's right
    //                 tree->_parent->_left = tree->_right;
    //                 if (tree->_right) {
    //                     tree->_right->_parent = tree->_parent;
    //                 }
    //             } else if (tree->_parent->_right == tree) {
    //                 // Deleting parent's right node
    //                 // Make parent's right node point to tree's right
    //                 tree->_parent->_right = tree->_right;
    //                 if (tree->_right) {
    //                     tree->_right->_parent = tree->_parent;
    //                 }
    //             }
    //             tree->_left = 0;
    //             tree->_right = 0;
    //             delete tree;
    //         } else {
    //             TreeNode *tmp = tree->_right;
    //             tree->_site = tree->_right->_site;
    //             tree->_left = tree->_right->_left;
    //             tree->_right = tree->_right->_right;
    //             delete tmp;
    //         }
    //     } else {
    //         // No children - easy case
    //         delete tree;
    //         // if (tree->_site) {
                
    //         // }
    //     }
    // }

    void setEvent(const Point &pt, Event *ev) {
        TreeNode *node = insert(pt);
        if (node && 0==node->_event) {
            node->_event = ev;
        }
    }

    TreeNode *insert(const Point &pt) {

        // Tree is empty
        if (_site == 0) {
            _site = new Point(pt);
            return this;
        }

        if (pt == *_site) return this;

        if (pt<*_site) {
            if (0 == _left) {
                _left = new TreeNode;
                _left->_parent = this;
            }
            return _left->insert(pt);
        } else {
            if (0==_right) {
                _right = new TreeNode;
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
    
private:
    Point *_site;
    Event *_event;
    TreeNode *_left, *_right, *_parent;
};

std::ostream &operator<<(std::ostream &out, const TreeNode &e) {
    out << "{";
    e.inorder(out);
    out << "}";
    return out;
}

// class BinaryTree {
// public:
//     BinaryTree() :_root(0) {}
//     ~BinaryTree() {
//         if (_root) {
//             delete _root;
//             _root = 0;
//         }
//     }
//     void insert(const Point &pt) {
//         if (_root == 0) {
//             _root = new TreeNode;
//         }
//         _root->insert(pt);
//     }
//     void setEvent(const Point &pt, Event *ev) {
//         if (_root == 0) {
//             _root = new TreeNode;
//         }
//         _root->setEvent(pt, ev);
//     }
//     const TreeNode *find(const Point &pt) {
//         if (_root == 0) {
//             return 0;
//         }
//         return _root->find(pt);
//     }
//     bool isEmpty() {
//         return _root == 0;
//     }
//     void remove(const Point &pt) {
//         if (_root == 0) return;
//         if (_root->site() == pt) {
//             // Deleting the root!
//             if (_root->_
//         }
//     }
//     void inorder(std::ostream &out) const {
//         if (_root) {
//             _root->inorder(out);
//         }
//     }
// private:
//     TreeNode *_root;
// };

// std::ostream &operator<<(std::ostream &out, const BinaryTree &e) {
//     out << "{";
//     e.inorder(out);
//     out << "}";
//     return out;
// }


typedef std::set<Point> point_set;
typedef std::vector<Edge> edge_list;
typedef std::priority_queue<Event*> event_queue;

void voronoi(const point_set &pts, edge_list &output) {
    std::cout << "Computing voronoi diagram for points:\n";
    
    event_queue eq;
    output.clear();
    TreeNode stat;

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
            if (stat.isEmpty()) {
                std::cout << "Empty status, inserting point.\n";
                stat.insert(thisEvent->point());
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
    TreeNode omg;
    for (const auto &p : pts) {
        omg.insert(p);
    }
    std::cout << omg << "\n";
    omg.remove(Point{3,4});
    std::cout << omg << "\n";
    omg.remove(Point{1,2});
    std::cout << omg << "\n";
    // omg.inorder(std::cout);
    // voronoi(pts, edges);

    return 0;
}
