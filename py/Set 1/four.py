'''One of the 60-character strings in this file has been encrypted by
single-character XOR.

Find it.
'''

import math
import urllib2
from binascii import unhexlify, hexlify

from three import LETTERS

FILE = "http://cryptopals.com/static/challenge-data/4.txt"


def strings():
    r = urllib2.urlopen(FILE)
    strings = r.read().split("\n")
    return strings


def entropy(string):
    '''Calculates the Shannon entropy of a string
    http://stackoverflow.com/questions/2979174/how-do-i-compute-the-approximate-entropy-of-a-bit-string
    '''

    # get probability of chars in string
    prob = [float(string.count(c)) / len(string) for c in dict.fromkeys(list(string))]

    # calculate the entropy
    entropy = - sum([p * math.log(p) / math.log(2.0) for p in prob])

    return entropy


if __name__ == "__main__":
    cts = [unhexlify(ct) for ct in strings()]
    cts = [(ct, entropy(ct)) for ct in cts]
    sorted_by_entropy = sorted(cts, key=lambda ct: ct[1])

    ct = hexlify(sorted_by_entropy[0][0])
    ent = sorted_by_entropy[0][1]

    print("I sorted the ciphertexts by their Shannon entropy.")
    print("Here are is the sample with the lowest entropy ({}):".format(ent))
    print(ct)
    print("Chances are, this is your guy. I cbf to check right now.")
    # The lowest entropy is 3.99, the entropy of CT from 3 is 3.914
    # the next highest is 4.319, and it doesn't change much more after that
    # so we're probably good
