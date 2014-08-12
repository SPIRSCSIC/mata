'''I'm killing your brain like a poisonous mushroom'''

import binascii
import base64


STR = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

def do_stuff():
    print(base64.b64encode(binascii.unhexlify(STR)))

if __name__ == "__main__":
    do_stuff()
