import hashlib
import hmac

def hmac_sha256_digest(key, msg):
    """
    Return the HMAC-SHA256 message authentication code of the message
    'msg' with key 'key'.
    """

    return hmac.new(key, msg, hashlib.sha256).digest()

AUTH_SERVER_TO_CLIENT_CONST = "ExtORPort authentication server-to-client hash"

def compl(s, nch):
  return s + reduce(lambda x, y: x + y, ['A'] * (nch - len(s)), "")


hval = "\233[\138\153f\242L\243\ESC\164\150K?\177\243m\144\133\171\f\207\215?V\197\GS\SOH\228)\DC1V%"

dig =  hmac_sha256_digest(compl("authCookie",32), 
  AUTH_SERVER_TO_CLIENT_CONST + compl("clientNonce", 32) + compl("serverNonce", 32))

f = open("digesthmac", "w")
f.write(dig)
