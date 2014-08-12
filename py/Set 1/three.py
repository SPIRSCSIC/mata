'''The hex encoded string:

1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
... has been XOR'd against a single character.
Find the key, decrypt the message.

You can do this by hand. But don't: write code to do it for you.

How? Devise some method for "scoring" a piece of English plaintext.
Character frequency is a good metric.
Evaluate each output and choose the one with the best score.
'''

from collections import defaultdict
from binascii import hexlify, unhexlify

from two import xor

CT = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
KEYS = [chr(i) for i in xrange(0x7f)]  # all possible keys
LETTERS = ("e", "t", "o", "a", "i", "n", "s", "r")  # most frequent letters


def decrypt(key, ct):
    '''pad the key to ct length and xor them together.
    ct and k are hex strings (hexlify output)
    '''

    ct = unhexlify(ct)
    k = unhexlify(key)

    key = ""
    diff = len(ct) - len(k)
    while diff > 0:
        key += k
        diff = len(ct) - len(key)
    assert len(ct) == len(key)
    return unhexlify(_hex(xor(hexlify(ct), hexlify(key))))


def _hex(byte):
    '''pad a hex string to a multiple of 2'''

    if len(byte) % 2 != 0:
        return "0" + byte
    else:
        return byte


def freqdist(ct):
    '''find the number of occurances of each byte in a hexstring.'''

    dist = defaultdict(int)
    bytess = [byte for byte in unhexlify(ct)]
    for byte in bytess:
        dist[byte] += 1
    return dist


def solve_single_char(ct):
    dist = freqdist(ct)
    sort = sorted(dist, key=dist.get)  # sort bytes by occurance, low to high

    most_frequent = sort[-1]
    keys = [_hex(xor(hexlify(most_frequent), hexlify(k))) for k in KEYS]

    garbage = [chr(i) for i in range(0x00, 0x41) + range(0x7b, 0x7f)]
    garbage.append(["[", "\\", "]", "^", "_", "`"])
    garbage.remove("'")
    garbage.remove(" ")
    possibilities = []

    for k in keys:
        pt = decrypt(k, CT).lower()
        dist = freqdist(pt.encode("hex"))
        sort = sorted(dist, key=dist.get)

        for char in sort:
            if char in garbage:
                try:
                    possibilities.remove((k, pt))
                except:
                    pass
                break
        else:
            possibilities.append((k, pt))

    return possibilities


if __name__ == "__main__":
    possibilities = solve_single_char(CT)

    print("possible keys and ciphertexts:")
    for i in possibilities:
        print("key: {}".format(unhexlify(i[0])))
        print(i[1])
