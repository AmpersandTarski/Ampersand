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
  )
where

import Ampersand.Basics.Prelude
import qualified Crypto.Hash as CH
import Data.Hashable
import qualified RIO.Text as T

sha1hash :: Text -> CH.Digest CH.SHA1
sha1hash text = CH.hash (T.encodeUtf8 text)
