import heapq

__author__ = 'Christopher Nelson'

import numpy as np


class AmortizedArray:
    """
    This object maintains several arrays, which are already sorted. The arrays have fixed size. Elements are
    migrated from array to array as the number of items grows. The arrays are always powers of two. Once you
    have enough full arrays to make the next power of two, a new array is created and the existing arrays
    are merged into the new array. The other arrays are marked empty. The cycle begins again.
    """

    def __init__(self, dtype):
        self.dtype = dtype
        self.arrays = [None]
        self.full = np.zeros(1, dtype=np.bool_)

    def __iter__(self):
        arrays = [a for a in self.arrays if a is not None]
        yield from heapq.merge(*arrays)

    def _grow(self, value):
        """
        This function is called when the current arrays are all filled up. It creates a new array, sorts the
        existing arrays into that, grows the present and full bitmaps, and resets the bits as needed.
        """
        self.arrays.append(np.array([value], dtype=self.dtype))
        new_array = np.concatenate(self.arrays)
        new_array.sort()
        self.full = np.zeros(len(self.full) + 1, dtype=np.bool_)
        self.full[-1] = True
        self.arrays = [None] * len(self.full)
        self.arrays[-1] = new_array

    def _shift(self, value):
        # The current slot is full. We need to merge everything from 0 to the next empty slot
        # and put it in a new array, set the full bit, and reset the other full bits.
        full_slots = []
        source_arrays = [np.array([value], dtype=self.dtype)]
        for i, v in enumerate(self.full):
            if not v:
                next_empty_slot = i
                break
            full_slots.append(i)

        for i in full_slots:
            source_arrays.append(self.arrays[i])

        #print("next=", next_empty_slot, "full=", full_slots, self.full, "concat=", source_arrays)
        new_array = np.concatenate(source_arrays)
        new_array.sort()
        self.arrays[next_empty_slot] = new_array
        self.full[next_empty_slot] = True
        for i in range(0, next_empty_slot):
            self.full[i] = False
            self.arrays[i] = None

    def append(self, value):
        if not self.full[0]:
            self.arrays[0] = np.array([value], dtype=self.dtype)
            self.full[0] = True
        elif np.count_nonzero(self.full) == len(self.full):
            self._grow(value)
        else:
            self._shift(value)


if __name__ == "__main__":
    import random

    a = AmortizedArray(np.int)
    c = random.sample(range(1, 50), 20)
    print(a.arrays)
    for v in c:
        a.append(v)
        print(a.arrays)

    sc = sorted(c)
    a2 = list(a)

    print(sc)
    print(a2)
    print(sc == a2)






