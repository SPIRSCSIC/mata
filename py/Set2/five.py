from urlparse import parse_qs
import re
from binascii import hexlify

from two import aes_ecb_encrypt, aes_ecb_decrypt
from three import gen_aes_key


def profile_for(email):
    email = email.replace("&", "").replace("=", "")
    return "email={}&uid=10&role=user".format(email)


def attack(profile):
    pass


if __name__ == "__main__":
    l = profile_for("lolrus")
    key = gen_aes_key()
    aes_ecb_encrypt(key, l)
    print("supplying: {}".format(hexlify(l)))
    print("received: {}".format(attack(l)))
