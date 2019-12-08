from dataclasses import dataclass, field
from collections import defaultdict

# TODO: Figure out how to achieve this in Haskell
# It's hella inefficeint because it does a greedy search on all the childrens
# children


@dataclass
class Node:
    value: str
    children: list = field(default_factory=list)

    @property
    def leafs(self):
        yield self.value
        for child in self.children:
            yield from child.leafs

    def distance_between(self, a, b):
        ancestor = next(tree.common_ancestor('SAN', 'YOU'))
        return ancestor.distance_to(a) +  ancestor.distance_to(b)


    def distance_to(self, pos, acc=0):
        for child in self.children:
            if child.value == pos:
                return acc
            if pos in child.leafs:
                return child.distance_to(pos, acc+1)

    def common_ancestor(self, a, b):
        # Do the same thing for all the children
        for child in self.children:
            if a in child.leafs and b in child.leafs:
                yield from child.common_ancestor(a, b)
        # If the current node has both a and b as a leaf node, then return it
        yield self


    def orbits(self, depth=0):
        yield depth
        for child in self.children:
            yield from child.orbits(depth+1)

def build_from(key):
    node = Node(value=key)
    if children := (orbit_map.get(key)):
         node.children = [build_from(child) for child in children]
    return node

if __name__ == "__main__":
    orbit_map = defaultdict(list)
    with open("inputs/day6.txt") as f:
        for parent, child in (line.strip().split(")") for line in f):
            orbit_map[parent].append(child)


    tree = build_from('COM')
    print(tree.distance_between('SAN', 'YOU'))
