from functools import reduce
import fileinput
import re
import sys

def part_one(numbers):
    circle = init_circle(numbers)
    return circle

def init_circle(numbers):
    size = len(numbers)
    a = [None]*(size)
    for i in range(0,size-1):
        a[numbers[i]-1] = numbers[i+1]
    a[numbers[size-1]-1] = numbers[0]
    return a

if __name__ == '__main__' and not sys.flags.interactive:
    [line] = [l.strip() for l in fileinput.input()]
    numbers = [int(x) for x in list(line)]
    print(part_one(numbers))
