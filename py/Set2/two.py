'''CBC mode is a block cipher mode that allows us to encrypt
irregularly-sized messages, despite the fact that a block cipher natively
only transforms individual blocks.

In CBC mode, each ciphertext block is added to the next plaintext block before
the next call to the cipher core.

The first plaintext block, which has no associated previous ciphertext block,
is added to a "fake 0th ciphertext block" called the initialization vector,
or IV.

Implement CBC mode by hand by taking the ECB function you wrote earlier,
making it encrypt instead of decrypt (verify this by decrypting whatever
you encrypt to test), and using your XOR function from the previous exercise
to combine them.

The file here is intelligible (somewhat) when CBC decrypted against
"YELLOW SUBMARINE" with an IV of all ASCII 0 (\x00\x00\x00 &c)

Do not use OpenSSL's CBC code to do CBC mode, even to verify your results.
What's the point of even doing this stuff if you aren't going to learn
from it?
'''

import sys, os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
import urllib2
import base64
from binascii import hexlify, unhexlify

from Crypto.Cipher import AES

from one import pad
from Set1.two import xor2 as xor

FILE = "http://cryptopals.com/static/challenge-data/10.txt"
def ciphertext():
    r = urllib2.urlopen(FILE)
    ct = r.read()
    return base64.b64decode(ct)
KEY = "YELLOW SUBMARINE"
_PT = pad("asdf")
IV = "\x00" * 16
CT = ciphertext()


def aes_ecb_encrypt(key, pt):
    aes = AES.new(key, mode=AES.MODE_ECB)
    return aes.encrypt(pad(pt))


def aes_ecb_decrypt(key, ct):
    aes = AES.new(key, mode=AES.MODE_ECB)
    return aes.decrypt(pad(ct))


def blocks(s, bs=16):
    return [''.join(x) for x in zip(*[list(s[z::bs]) for z in range(bs)])]


def aes_cbc_encrypt(key, pt, iv):
    ct = ""
    pt_blocks = blocks(pt)
    init = aes_ecb_encrypt(key, xor(iv, pt_blocks[0]))
    ct += init
    past = init
    for block in pt_blocks[1:]:
        past = aes_ecb_encrypt(key, xor(block, past))
        ct += past
    return ct


def aes_cbc_decrypt(key, ct, iv):
    pt = ""
    ct_blocks = blocks(ct)
    pt += xor(iv, aes_ecb_decrypt(key, ct_blocks[0]))  # init round
    past = ct_blocks[0]
    for block in ct_blocks[1:]:
        pt += xor(past, aes_ecb_decrypt(key, block))
        past = block
    return pt


if __name__ == "__main__":
    assert aes_ecb_decrypt(KEY, aes_ecb_encrypt(KEY, _PT)) == _PT
    pt = aes_cbc_decrypt(KEY, CT, IV)
