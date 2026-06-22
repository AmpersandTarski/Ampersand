-- | In the ampersand codebase we currently use two types of hashing.
--   It started long time ago, when there was a need to hash things. In
--   that time, the choice was made for the package `hashable ` (https://hackage.haskell.org/package/hashable).
--   Recently, there is a need to use SHA1 hashing. When namespaces were introduced,
--   we used hashes to generate a specific prefix for the names of tables and columns in sql plugs.
--   we need to be absolutely sure that the names are unique in their contexts.
--   We also do not want these generated names change when we update the version of a package. This isn't
--   guaranteed by the package `hashable`.
--   So we turned to `cryptonite`, because it supplies SHA1 hashing. However, it doesn't supply hashWithSalt,
--   which we use a lot. Maybe one day we will replace hashable, but for now it is kept as-is.
module Ampersand.Basics.Hashing
  ( -- * from package `hashable`
    hash,
    hashWithSalt,

    -- * Based on the package `cryptonite`
    sha1hash,

    -- * Object-identity shortening (cross-repo contract, see below)
    shortenObjectId,
  )
where

import Ampersand.Basics.Prelude
import qualified Crypto.Hash as CH
import Data.Hashable
import qualified RIO.Text as T

sha1hash :: Text -> CH.Digest CH.SHA1
sha1hash = CH.hash . T.encodeUtf8

-- | Shorten an OBJECT atom identity so it stays unique within 254 characters.
--
--   Object identities are opaque labels, stored in a VARCHAR(255) column and required to be
--   unique within their concept. An id of at most 254 characters is kept verbatim; a longer id
--   is deterministically shortened to @<first 243 chars>_<10 hex chars of sha1(id)>@ (254 chars).
--   Deterministic on purpose: every reference to the same id maps to the same atom, while
--   distinct ids stay distinct (the hash is taken over the full id).
--
--   CONTRACT: this MUST stay byte-for-byte identical to the runtime framework's object-id
--   shortening in PrototypeFramework: backend/src/Ampersand/Core/Atom.php (Atom::setId, OBJECT
--   case). Compiler-generated population (database.sql) and runtime-imported atoms must map to
--   the same database row. Shared known-answer vector: "a" x 300 -> "a" x 243 <> "_003ef1ba9e".
shortenObjectId :: Text -> Text
shortenObjectId str
  | T.length str > 254 = T.take 243 str <> "_" <> T.take 10 (T.pack (show (sha1hash str)))
  | otherwise = str
