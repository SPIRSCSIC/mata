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

from three import solve_single_char, decrypt
from five import pad_key

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

    x = int(hexlify(x), 16)
    y = int(hexlify(y), 16)

    count,z = 0,x^y
    while z:
        count += 1
        z &= z-1 # magic!
    return count


def find_keysize(ct):
    sizes = defaultdict(int)
    # Let KEYSIZE be the guessed length of the key; try values from 2 to 40.
    for size in xrange(2, 40):
        # For each KEYSIZE, take the first KEYSIZE worth of bytes.
        first = ct[0:size]
        assert len(first) == size
        # and the second KEYSIZE worth of bytes
        second = ct[size:(2 * size)]
        third = ct[size:(3 * size)]
        fourth = ct[size:(4 * size)]
        # and find the edit distance between them
        # Normalize this result by dividing by KEYSIZE.
        # You could proceed perhaps with the smallest 2-3 KEYSIZE values.
        # Or take 4 KEYSIZE blocks instead of 2 and average the distances
        distance1 = hamming(first, second) / float(size)
        distance2 = hamming(third, fourth) / float(size)
        distance3 = hamming(first, third) / float(size)
        distance4 = hamming(second, fourth) / float(size)
        sizes[size] = sum((distance1, distance2, distance3, distance4)) / 4

    # The KEYSIZE with the smallest normalized edit distance is probably the key
    sort = sorted(sizes, key=sizes.get)
    return sort


def transpose(blocks):
    #return ["".join(i) for i in zip(*blocks)]
    transposed = defaultdict(str)
    for block in blocks:
        for idx, char in enumerate(block):
            transposed[idx] += char
    return transposed


def crack_vigenere(ct, keysize=None):
    if not keysize:
        keysize = find_keysize(ct)[0]

    # Now that you probably know the KEYSIZE: break the ciphertext into blocks
    # of KEYSIZE length.
    split_ct = split_str(ct, keysize)
    # Now transpose the blocks
    transposed = transpose(split_ct).values()
    assert len(transposed) == keysize
    # Solve each block as if it was single-character XOR
    keys = [solve_single_char(hexlify(t)) for t in transposed]
    keys = [k[0][0] for k in keys]
    # For each block, the single-byte XOR key that produces the best looking
    # histogram is the repeating-key XOR key byte for that block. Put them
    # together and you have the key.
    key = unhexlify("".join(keys))

    # now we try to decrypt
    _key = pad_key(key, len(ct))
    return key, decrypt(hexlify(_key), hexlify(ct), pad_key=False)


if __name__ == "__main__":
    assert hamming("this is a test", "wokka wokka!!!") == 0x25
    assert transpose(["111", "222", "333"]).values()  == ["123", "123", "123"]
    k, ct = crack_vigenere(CT)
    print("Key: {}".format(k))
    print(ct)
