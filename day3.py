from typing import NamedTuple

class Point(NamedTuple):
    x: int
    y: int

    def __repr__(self):
        return f'({self.x},{self.y})'


def draw_wire(instructions):
    # This was written in anger, don't judge
    points = []
    pos = Point(0, 0)
    for instruction in instructions:
        direction = instruction[0]
        num = int(instruction[1:])
        # X Axis == LEFT RIGHT

        if direction == 'R':
            for i in range(0, num+1):
                next_pos = Point(pos.x+i, pos.y)
                points.append(next_pos)

        if direction == 'L':
            for i in range(0, num+1):
                next_pos = Point(pos.x-i, pos.y)
                points.append(next_pos)

        if direction == 'U':
            for i in range(0, num+1):
                next_pos = Point(pos.x, pos.y+i)
                points.append(next_pos)

        if direction == 'D':
            for i in range(0, num+1):
                next_pos = Point(pos.x, pos.y-i)
                points.append(next_pos)

        pos = next_pos
    return points


def to_set(points):
    return {(p.x, p.y) for p in points if p.x != 0 and p.y != 0}

if __name__ == "__main__":
    with open("inputs/day3.txt") as f:
        wire_a, wire_b = [s.split(",") for s in f.read().strip().split()]
    a_points = draw_wire(wire_a)
    b_points = draw_wire(wire_b)

    steps = ((a_points.index(i), b_points.index(i)) for i in to_set(a_points) & to_set(b_points))
    print(list(steps))
    # print(min(map(sum, intersections)))
