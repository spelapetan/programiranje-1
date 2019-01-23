# 3. naloga

# a)

def simetricen (x):
    if x == x[::-1]:
        return True
    else:
        return False

# b)
from functools import lru_cache

@lru_cache(maxsize=None)
def stevilo_delov (w, is_symmetric) :
    if w == []:
        return 0
    elif is_symmetric(w):
        return 1
    options = [number_of_blocks(w[:i], is_symmetric) +
               number_of_blocks(w[i:], is_symmetric) for i in range(1, len(w))]
    n = min(options)
    return n
