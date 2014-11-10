'''The hex encoded string:

1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
... has been XOR'd against a single character.
Find the key, decrypt the message.

You can do this by hand. But don't: write code to do it for you.

How? Devise some method for "scoring" a piece of English plaintext.
Character frequency is a good metric.
Evaluate each output and choose the one with the best score.
'''

from collections import defaultdict, Counter
from binascii import hexlify, unhexlify

from two import xor

CT = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
KEYS = [chr(i) for i in xrange(0x7f)]  # all possible keys
# http://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
ENGLISH_FREQ_DIST = {"E": .12, "T": .091, "A": .0812, "O": .0768, "I": .0731,
                     "N": .0695, "S": .0628, "R": .0602, "H": .0592, "D": .0432,
                     "L": .0398, "U": .0288, "C": .0271, "M": .0261, "F": .023,
                     "Y": .0211, "W": .0209, "G": .0203, "P": .0182, "B": .0149,
                     "V": .0111, "K": .0069, "X": .0017, "Q": .0011, "J": .001,
                     "Z": .0007, " ": .2}


def english_proximity(pt):
    dist = Counter(pt)
    score = 0.0

    for k, v in dist.iteritems():
        k = k.upper()
        score += ENGLISH_FREQ_DIST.get(k, 0) * v
    return score / len(pt)


def decrypt(key, ct, pad_key=True):
    '''pad the key to ct length and xor them together.
    ct and k are hex strings (hexlify output)
    '''

    ct = unhexlify(ct)
    k = unhexlify(key)

    if pad_key:
        key = ""
        diff = len(ct) - len(k)
        while diff > 0:
            key += k
            diff = len(ct) - len(key)
        assert len(ct) == len(key)
    else:
        key = k
    return unhexlify(_hex(xor(hexlify(ct), hexlify(key))))


def _hex(byte):
    '''pad a hex string to a multiple of 2'''

    if len(byte) % 2 != 0:
        return "0" + byte
    else:
        return byte


def solve_single_char(ct, keys=None, min_ep=0.05):
    '''Return a list of possible keys and the equivalent plaintext,

    (hopefully) ordered by likelyhood.
    '''

    if not keys:
        keys = KEYS
    keys = [hexlify(k) for k in keys]
    possibilities = []

    for k in keys:
        pt = decrypt(k, ct).lower()
        ep = english_proximity(pt)
        if ep >= min_ep:
            possibilities.append((k, pt, ep))

    return sorted(possibilities, key=lambda x: x[2])[::-1]


if __name__ == "__main__":
    possibilities = solve_single_char(CT)

    print("possible keys and ciphertexts:")
    for k, pt, ep in possibilities:
        print("key: {}".format(unhexlify(k)))
        print(pt)
        print("{}% sure.".format(ep * 100))
