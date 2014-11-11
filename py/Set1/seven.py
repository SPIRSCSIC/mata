'''The Base64-encoded content in this file has been encrypted via AES-128
in ECB mode under the key "YELLOW SUBMARINE".

(case-sensitive, without the quotes; exactly 16 characters;
I like "YELLOW SUBMARINE" because it's exactly 16 bytes long,
and now you do too).

Decrypt it. You know the key, after all.

Easiest way: use OpenSSL::Cipher and give it AES-128-ECB as the cipher.
'''

import urllib2
import base64

try:
    from Crypto.Cipher import AES
except ImportError:
    print("sudo pip install pycrypto")
    exit()

FILE = "http://cryptopals.com/static/challenge-data/7.txt"


def ciphertext():
    r = urllib2.urlopen(FILE)
    ct = r.read()
    return base64.b64decode(ct)
CT = ciphertext()
KEY = "YELLOW SUBMARINE"


def aes_ecb_decrypt(key, ct):
    '''Decrypt a ciphertext with the given key.'''

    cipher = AES.new(key, mode=AES.MODE_ECB)
    return cipher.decrypt(ct)


if __name__ == "__main__":
    print(aes_ecb_decrypt(KEY, CT))
