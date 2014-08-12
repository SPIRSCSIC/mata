'''It is officially on, now.

This challenge isn't conceptually hard, but it involves actual error-prone coding.
The other challenges in this set are there to bring you up to speed.
This one is there to qualify you.
If you can do this one, you're probably just fine up to Set 6.
'''

import urllib2
from binascii import hexlify, unhexlify
from collections import defaultdict
import base64

FILE = "http://cryptopals.com/static/challenge-data/6.txt"


def ciphertext():
    r = urllib2.urlopen(FILE)
    ct = r.read()
    return base64.b64decode(ct)
CT = ciphertext()


def split_str(str, s_size):
    '''split a string into chunks of size s_size'''
    return [str[i:i + s_size] for i in range(0, len(str), s_size)]


def hamming(x, y):
    """Calculate the Hamming distance between two bit strings
    http://jhafranco.com/2012/02/12/hamming-distance/
    """
    assert len(x) == len(y)
    x = int(hexlify(x), 16)
    y = int(hexlify(y), 16)

    count,z = 0,x^y
    while z:
        count += 1
        z &= z-1 # magic!
    return count


def find_keysize(give=None):
    ct = CT
    sizes = defaultdict(int)
    for size in xrange(2, 40):
        first = CT[0:size - 1]
        second = CT[size:(2 * size - 1)]
        distance = hamming(first, second) / size
        sizes[size] = distance

    sort = sorted(sizes, key=sizes.get)

    if give:
        return sort[0:give]
    else:
        return sort[0]


def crack_this_shit(ct, keysize):
    split_ct = split_str(ct, keysize)


if __name__ == "__main__":
    assert hamming("this is a test", "wokka wokka!!!") == 0x25
    keysize = find_keysize()
    print(crack_this_shit(CT, keysize))
