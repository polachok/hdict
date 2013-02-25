module StarDict.Format.Dict where
import Data.Binary
import Data.Binary.Get
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (ByteString)

lookup :: (Integer, Integer) -> ByteString -> Text
lookup (off, len) = 
    let go = do
            skip (fromIntegral off)
            getByteString (fromIntegral len)
    in
    decodeUtf8 . runGet go
