'''In this file are a bunch of hex-encoded ciphertexts.

One of them has been encrypted with ECB.
Detect it.
Remember that the problem with ECB is that it is stateless and deterministic;
the same 16 byte plaintext block will always produce the same 16 byte
ciphertext.
'''

import urllib2
from binascii import unhexlify, hexlify
from collections import Counter

FILE = "http://cryptopals.com/static/challenge-data/8.txt"
def ciphertexts():
    r = urllib2.urlopen(FILE)
    ct = r.read()
    return ct.split("\n")[:-1]  # last is an empty string
CTS = ciphertexts()


def is_ecb(ct):
    '''Try to detect an ECB-mode encrypted ciphertext.'''

    # split ct in 128-bit (16 byte) blocks
    blocks = [''.join(x) for x in zip(*[list(ct[z::16]) for z in range(16)])]
    # compare number of unique blocks to number of blocks
    unique = Counter(blocks)
    if len(unique.keys()) < len(blocks):
        return True
    else:
        return False


if __name__ == "__main__":
    ecb = [ct for ct in CTS if is_ecb(ct)]
    if len(ecb) == 1:
        print(ecb)[0]
    else:
        print("uh oh")
