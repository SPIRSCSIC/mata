'''Write a function that takes two equal-length buffers
and produces their XOR combination.
'''

import binascii

ONE = "1c0111001f010100061a024b53535009181c"
TWO = "686974207468652062756c6c277320657965"
XORED = "746865206b696420646f6e277420706c6179"


def xor(one, two):
    '''xor 2 hex strings'''

    assert len(one) == len(two)
    one = int(one, 16)
    two = int(two, 16)

    xored = one ^ two
    # hex's output is a bit goofy
    return hex(xored).replace("0x", "").replace("L", "")


def xor2(one, two):
    '''xor two non-hex encoded strings'''

    out = xor(binascii.hexlify(one), binascii.hexlify(two))
    try:
        return binascii.unhexlify(out)
    except:
        return binascii.unhexlify("0" + out)

if __name__ == "__main__":
    try:
        assert xor(ONE, TWO) == XORED
    except AssertionError:
        print("Failure!")
    else:
        print("Success!")
