import base64
from functools import partial
from collections import Counter
from binascii import hexlify, unhexlify

from Set1.eight import is_ecb
from two import aes_ecb_encrypt
from three import gen_aes_key


PAD = """Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
         aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
         dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
         YnkK""".strip()
KEY = gen_aes_key()
#KEY = unhexlify("c2e7b8ae53a2860ecbc62e33d30a5954")

def pad(a, bs=16, pad="\x04"):
    while len(a) % bs != 0:
        a += pad
    return a


def padding_oracle(pt, key, pad):
    pt = pt + pad
    return aes_ecb_encrypt(pt, key)
oracle = partial(padding_oracle, key=KEY, pad=base64.b64decode(PAD))


def break_padding_oracle(oracle):
    # find block size
    bs = None
    byte = "A"
    for c in xrange(1,33):  # we're going to assume max block size is 32
        pt = byte * c
        ct = oracle(pt)
        if c == 15:
            print Counter(ct)
        if c == 16:
            print Counter(ct)

    # detect ECB



if __name__ == "__main__":
    print(break_padding_oracle(oracle))
