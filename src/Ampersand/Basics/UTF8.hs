{-
Copyright (C) 2010 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.UTF8
   Copyright   : Copyright (C) 2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

UTF-8 aware string IO functions that will work with GHC 6.10, 6.12, or 7.
-}
module Ampersand.Basics.UTF8
           ( readUTF8File
           , readFile
           , writeFile
           , getContents
           , putStr
           , putStrLn
           , hGetContents
           , hPutStr
           , hPutStrLn
           , stdout
           , BufferMode(..)
           , hSetBuffering 
           )

where
import Codec.Binary.UTF8.String (encodeString)
import qualified Data.ByteString as B hiding (putStrLn)
import qualified Data.ByteString.Char8 as C (putStrLn)
import Data.ByteString.UTF8 (toString, fromString)
import Prelude hiding (readFile, writeFile, getContents, putStr, putStrLn)
import System.IO (Handle,stdout, BufferMode(..),hSetBuffering )
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Data.Bits
import Control.Exception

bom :: B.ByteString
bom = B.pack [0xEF, 0xBB, 0xBF]

stripBOM :: B.ByteString -> B.ByteString
stripBOM s | bom `B.isPrefixOf` s = B.drop 3 s
stripBOM s = s

-- Try to read file pth as UTF-8 and return (Left err) in case of failure, or (Right contents) on success.
readUTF8File :: FilePath -> IO (Either String String)
readUTF8File pth =
 do { contents <- stripBOM <$> B.readFile pth
    -- Exceptions from decodeUtf8 only show the offending byte, which is not helpful, so we validate the file ourselves to get a good error message.
    ; let res = case validateUTF8 contents of
                 Just utf8PrefixRev -> let utf8Lines = lines . unpack . decodeUtf8 $ utf8PrefixRev
                                       in  Left $ "Invalid UTF-8 character at line "++ show (length utf8Lines) ++
                                           case reverse utf8Lines of 
                                             []   -> "" -- won't happen
                                             ln:_ -> " : " ++ show (length ln + 1) ++ " (column nr when viewed as UTF-8)\n" ++ 
                                                     "Text preceding invalid character:\n" ++ "..." ++ (reverse . take 50 . reverse $ ln)++"<INVALID CHARACTER>" 
                 Nothing -> let txt = decodeUtf8 contents
                            in  Right $ unpack txt
    ; seq (either length length res) $ return res -- force decodeUtf8 exceptions
    } `catch` \exc ->
    return $ Left $ show (exc :: SomeException) --  should not occur if validateUTF8 works correctly
   

readFile :: FilePath -> IO String
readFile = fmap (toString . stripBOM) . B.readFile . encodeString

writeFile :: FilePath -> String -> IO ()
writeFile f = B.writeFile (encodeString f) . fromString

getContents :: IO String
getContents = fmap (toString . stripBOM) B.getContents

putStr :: String -> IO ()
putStr = B.putStr . fromString

putStrLn :: String -> IO ()
putStrLn = C.putStrLn . fromString

hGetContents :: Handle -> IO String
hGetContents h = fmap (toString . stripBOM) (B.hGetContents h)

hPutStr :: Handle -> String -> IO ()
hPutStr h = B.hPutStr h . fromString

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h (s ++ "\n")

-- Return Nothing if bs is valid UTF-8, or Just the maximum valid prefix if it contains an invalid character
validateUTF8 :: B.ByteString -> Maybe B.ByteString
validateUTF8 bs = fmap (B.pack . reverse) $ validate [] $ B.unpack bs
  where validate :: [Word8] -> [Word8] -> Maybe [Word8]
        validate _         []                                     = Nothing
        validate validated (w:ws)            | bitMask0xxxxxxx w  = validate (w : validated) ws
        validate validated (w1:w2:ws)        | bitMask110xxxxx w1 
                                            && bitMask10xxxxxx w2 = validate (w2:w1 : validated) ws
        validate validated (w1:w2:w3:ws)     | bitMask1110xxxx w1
                                            && bitMask10xxxxxx w2
                                            && bitMask10xxxxxx w3 = validate (w3:w2:w1 : validated) ws
        validate validated (w1:w2:w3:w4:ws)  | bitMask11110xxx w1
                                            && bitMask10xxxxxx w2
                                            && bitMask10xxxxxx w3
                                            && bitMask10xxxxxx w4 = validate (w4:w3:w2:w1 : validated) ws
        validate validated _                                      = Just validated
        
        bitMask0xxxxxxx w = w .&.  bit 7                                  == 0
        bitMask10xxxxxx w = w .&. (bit 7 + bit 6)                         == bit 7
        bitMask110xxxxx w = w .&. (bit 7 + bit 6 + bit 5)                 == bit 7 + bit 6
        bitMask1110xxxx w = w .&. (bit 7 + bit 6 + bit 5 + bit 4)         == bit 7 + bit 6 + bit 5
        bitMask11110xxx w = w .&. (bit 7 + bit 6 + bit 5 + bit 4 + bit 3) == bit 7 + bit 6 + bit 5 + bit 4
