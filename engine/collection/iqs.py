__author__ = 'Christopher Nelson'


def quick_select(arr, k):
    if arr is None or len(arr) <= k:
        raise IndexError()

    from_idx = 0
    to_idx = len(arr) - 1

    # If from_idx == to_idx we reached the kth element
    while from_idx < to_idx:
        r = from_idx
        w = to_idx
        mid = arr[(r + w) // 2]

        # Stop if the reader and writer meets
        while r < w:

            if arr[r] >= mid:
                # Put the large values at the end
                arr[w], arr[r] = arr[r], arr[w]
                w -= 1
            else:
                # The value is smaller than the pivot, skip
                r += 1

        # If we stepped up (r++) we need to step one down
        if arr[r] > mid:
            r -= 1

        # The r pointer is on the end of the first k elements
        if k <= r:
            to_idx = r
        else:
            from_idx = r + 1

    return arr[k]


def incremental_quick_select(a):
    stack = []
    idx = len(a) - 1
    while True:
        stack.append(idx)
        value = a[idx]

