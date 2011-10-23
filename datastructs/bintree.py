#!/usr/bin/env python3

# bintree.py

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

# Simple unbalanced binary tree
# Normally I'd use a library for this, but the whole point of this project
# is reinventing the wheel for learning, so might as well go all the way...

class Node(object):
    def __init__(self, val, ltf = None):
        self.left = None
        self.right = None
        self.parent = None
        self.value = val
        if ltf:
            self.lt = ltf
        else:
            self.lt = type(val).__lt__

    def rightMost(self):
        if self.right is None: return self
        pn = self.right
        cn = self.right
        while cn:
            pn = cn
            cn = cn.right
        return pn

    def leftMost(self):
        if self.left is None: return self
        pn = self.left
        cn = self.left
        while cn:
            pn = cn
            cn = cn.left
        return pn

    def next(self):
        if self.right: return self.right.leftMost()
        rn = self.parent
        while rn and self.lt(rn.value, self.value):
            rn = rn.parent
        return rn

    def prev(self):
        if self.left: return self.left.rightMost()
        rn = self.parent
        while rn and not self.lt(rn.value, self.value):
            rn = rn.parent
        return rn

    def isLeftOfParent(self):
        if self.parent is None: raise Exception('Node has no parent')
        return self.lt(self.value, self.parent.value)

    def __str__(self):
        rv = '('

        if self.parent:
            rv += str(self.parent.value) +', '
        else:
            rv += 'None, '

        if self.left:
            rv += str(self.left.value) + ', '
        else:
            rv += 'None, '

        rv += str(self.value) + ', '

        if self.right:
            rv += str(self.right.value)
        else:
            rv += 'None'

        rv += ')'
        return rv

class BinaryTree(object):
    def __init__(self, cf = None):
        self.root = None
        self.numElems = 0
        if cf:
            self.lt = cf

    def lt(self, a, b):
        return a<b

    def size(self):
        return self.numElems

    def insert(self, val):
        if self.root is None:
            self.root = Node(val, self.lt)
            self.numElems += 1
            return

        if self.root.value == val:
            return

        prevNode = None
        curNode = self.root
        while curNode:
            if val == curNode.value: return
            prevNode = curNode
            if self.lt(val, curNode.value):
                curNode = curNode.left
            else:
                curNode = curNode.right

        if val==prevNode.value: return
        nn = Node(val, self.lt)
        self.numElems += 1
        if self.lt(val, prevNode.value):
            prevNode.left = nn
            nn.parent = prevNode
        else:
            prevNode.right = nn
            nn.parent = prevNode

    def findNode(self, val):
        if self.root is None: return None
        if self.root.value == val:
            return self.root

        curNode = self.root
        while curNode and curNode.value != val:
            if self.lt(val, curNode.value):
                curNode = curNode.left
            else:
                curNode = curNode.right
        return curNode

    def contains(self, val):
        nd = self.findNode(val)
        return nd is not None
        
    def inOrderVisit(self, func):
        if self.root is None: return
        cn = self.root
        if cn:
            cn = cn.leftMost()
        while cn:
            func(str(cn))
            cn = cn.next()

    def preOrderVisit(self, func):
        def innerPre(node):
            if node:
                func(node.value)
                innerPre(node.left)
                innerPre(node.right)
        innerPre(self.root)

    def removeVal(self, val):
        rn = self.findNode(val)
        self.removeNode(rn)

    def showTree(self):
        def innerShow(val):
            print(val, end=' ')
            
        print('Tree is: ', end=' ')
        self.inOrderVisit(innerShow)
        print()

    def __str__(self):
        if self.root is None: return '{}'
        rv = '{'
        cn = self.root
        if cn:
            cn = cn.leftMost()
        while cn:
            rv += str(cn)
            cn = cn.next()
            if cn: rv += ', '
        return rv + '}'

    def __repr__(self):
        return str(self)

    def removeNode(self, node):
        if node is None or self.root is None:
            return

        # Both children empty - link from parent becomes None
        if node.left is None and node.right is None:
            if node.parent:
                self.numElems -= 1
                if node.isLeftOfParent():
                    node.parent.left = None
                else:
                    node.parent.right = None
            else:
                self.root = None
            return

        # Node has only a left child - parent points to it
        elif node.left is not None and node.right is None:
            if node.parent:
                self.numElems -= 1
                if node.isLeftOfParent():
                    node.parent.left = node.left
                    node.left.parent = node.parent
                else:
                    node.parent.right = node.left
                    node.left.parent = node.parent
            else:
                self.root = node.left
                self.root.parent = None
            return

        # Node has only a right child - parent points to it
        elif node.left is None and node.right is not None:
            if node.parent:
                self.numElems -= 1
                if node.isLeftOfParent():
                    node.parent.left = node.right
                    node.right.parent = node.parent
                else:
                    node.parent.right = node.right
                    node.right.parent = node.parent
            else:
                self.root = node.right
                self.root.parent = None
            return

        
        # Node has both right and left child, replace node.value with next 
        nn = node.next()
        node.value = nn.value
        self.removeNode(nn)

    def min(self):
        if self.root is None: return None
        return self.root.leftMost()

    def popLowest(self):
        rv = self.min()
        if rv:
            self.removeNode(rv)
            return rv.value
        else:
            return None

def main():
    bob = BinaryTree(int.__gt__)
    # bob = BinaryTree()
    bob.insert(7)
    bob.insert(1)
    bob.insert(5)
    bob.insert(4)
    bob.insert(6)
    bob.insert(0)
    bob.insert(8)
    bob.insert(2)
    bob.insert(3)

    bob.showTree()
    print('tree has size', bob.size())
    print('Removing 5')
    bob.removeVal(5)
    print('tree has size', bob.size())
    print('Removed 5')
    bob.showTree()
    bob.removeVal(7)
    print('tree has size', bob.size())
    
    print('Removed 7')
    bob.showTree()
    print(bob.findNode(3).value)
    print(bob.findNode(3).next().value)
    print(bob.findNode(3).prev().value)
    bob.insert(-1)
    bob.insert(9)
    print('tree has size', bob.size())
    print(bob.findNode(0).prev().value)
    

if __name__=='__main__':
    main()
    
