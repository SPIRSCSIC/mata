import sys, os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
import base64
from functools import partial
from collections import Counter
from binascii import hexlify, unhexlify

from Set1.eight import is_ecb
from two import aes_ecb_encrypt, blocks
from three import gen_aes_key


PAD = base64.b64decode("""Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9
                          wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2
                          lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvI
                          HNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3Qg
                          ZHJvdmUgYnkK""".strip())
KEY = gen_aes_key()
#KEY = unhexlify("c2e7b8ae53a2860ecbc62e33d30a5954")


def oracle(pt, key, pad):
    pt = pt + pad
    return aes_ecb_encrypt(key, pt)


def find_oracle_blocksize(oracle):
    bs = None
    pad_len = len(oracle(""))
    pt = ""
    for c in xrange(1,33):  # we're going to assume max block size is 32
        ct = oracle("A" * c)
        if len(ct) > pad_len:
            bs = len(ct) - pad_len
            break
    return bs


def find_oracle_plaintext_size(oracle):
    base = oracle("")
    for i in range(16):
        pt = "A" * i
        ct = oracle(pt)
        if len(ct) > len(base):
            return len(base) - i


def find_next_byte(pad, block, oracle, bs=16):
    pt = "A" * (bs - 1 - len(pad))
    pt += pad
    for i in xrange(0x100):
        _pt = pt + chr(i)
        _ct = oracle(_pt)
        if blocks(_ct, bs=bs)[0] == block:
            return chr(i)
    return "?"


def break_oracle(oracle):
    bs = find_oracle_blocksize(oracle)
    pt_size = find_oracle_plaintext_size(oracle)
    ecb = is_ecb(oracle("A" * 32))
    pt = ""
    for _ in xrange(pt_size):
        pad_size = (bs - 1) - (len(pt) % bs)
        pad = "A" * pad_size
        out = oracle(pad)
        idx = len(pt) // bs
        block = blocks(out, bs=bs)[idx]
        prefix = (pad + pt)[-bs:]
        pt += find_next_byte(prefix, block, oracle, bs=bs)
    return pt


if __name__ == "__main__":
    oracle = partial(oracle, key=KEY, pad=PAD)
    assert find_oracle_blocksize(oracle) == 16
    print(break_oracle(oracle))
