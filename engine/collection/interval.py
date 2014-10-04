import bisect

__author__ = 'Christopher Nelson'


class Interval:
    @staticmethod
    def find(a, v):
        """
        Finds `v` in the interval list `a`. The list must be in sorted order of the kind created by using
        :func:`Interval.insert` below.

        :param a: An array (list) that is empty or contains intervals of the form (min,max) in sorted order.
        :param v: The value to find in the list.
        :return: The index if found, or None if not.
        """
        x = (v, v)
        i = bisect.bisect_left(a, x)
        if i == len(a):
            return None

        pl = a[i]
        if pl[0] <= v <= pl[1]:
            return i

        return None

    @staticmethod
    def _check_left(a, i, v, x):
        # Check the interval to the left.
        i2 = i - 1
        pl = a[i2]

        # This value is already in the interval
        if pl[0] <= v <= pl[1]:
            return a

        # See if we can extend the interval's upper bound
        dist = v - pl[1]
        if dist == 1:
            a[i2] = (pl[0], v)
            return a

        a.insert(i, x)
        return a

    @staticmethod
    def insert(a, v):
        """
        This function inserts value `v` into the interval list, such that the list maintains its sorted invariant.
        If the value can extend one of the existing intervals we do that rather than adding a new interval.

        :param a: An array (list) that is empty or contains intervals of the form (min,max) in sorted order.
        :param v: The value to insert into the list.
        :return: The array `a`.
        """
        x = (v, v)
        if len(a) == 0:
            a.append(x)
            return a

        i = bisect.bisect_left(a, x)
        if i == len(a):
            return Interval._check_left(a, i, v, x)

        pl = a[i]

        # This value is already in the interval
        if pl[0] <= v <= pl[1]:
            return a

        # See if we can extend the interval's lower bound
        dist = pl[0] - v
        if dist == 1:
            a[i] = (v, pl[1])
            return a

        if pl[0] > v:
            if i == 0:
                a.insert(0, x)
                return a

        return Interval._check_left(a, i, v, x)
