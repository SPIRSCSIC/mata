'''Now that you have ECB and CBC working:

Write a function to generate a random AES key;
that's just 16 random bytes.

Write a function that encrypts data under an unknown key ---
that is, a function that generates a random key and encrypts under it.

The function should look like:

encryption_oracle(your-input)
=> [MEANINGLESS JIBBER JABBER]
Under the hood, have the function append 5-10 bytes (count chosen randomly)
before the plaintext and 5-10 bytes after the plaintext.

Now, have the function choose to encrypt under ECB 1/2 the time,
and under CBC the other half (just use random IVs each time for CBC).
Use rand(2) to decide which to use.

Detect the block cipher mode the function is using each time.
You should end up with a piece of code that, pointed at a block box that
might be encrypting ECB or CBC, tells you which one is happening.
'''

import os

from Crypto.Cipher import AES


def gen_aes_key(block_size=16):
    return os.urandom(block_size)  # be sure to use kernel entropy


def encryption_oracle(pt, key=None, mode=None):
    if not key:
        key = gen_aes_key()
    if not mode:  # choose mode
        mode = random.choice([AES.MODE_ECB, AES.MODE_CBC])

    # append 5-10 bytes before and after
    pad_len = os.random.randrange(5,10)
    pad = os.urandom(pad_len)
    pt = "{0}{1}{0}".format(pad, pt)

    if mode == AES.MODE_ECB:
        return aes_ecb_encrypt(key, pt)
    elif mode == AES.MODE_CBC:
        iv = os.urandom(16)
        return aes_cbc_encrypt(key, pt, iv)
