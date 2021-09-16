module Cipher where

import Data.Char

cipherChar :: Int -> Char -> Char
cipherChar n c =
  let baseIndex = if isUpper c then ord 'A' else ord 'a'
      index = ord c - baseIndex
      newIndex = mod (index + n) 26
  in chr $ newIndex + baseIndex

cipher n = map $ cipherChar n

unCipherChar n = cipherChar (26 - mod n 26)

unCipher n = map $ unCipherChar n
