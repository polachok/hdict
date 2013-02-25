module StarDict.Format.Idx (parse32,parse64) where
import Data.Binary
import Data.Binary.Get
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad
import Control.Monad.Writer
import qualified Data.Map as Map

data Entry32 = Entry32 !Text !Integer !Integer
data Entry64 = Entry64 !Text !Integer !Integer

zeroTerminatedUtf8String =
    let go = do
         c <- lift getWord8
         when (c /= 0) $ tell [c] >> go
    in
    liftM (decodeUtf8 . BS.pack) $ execWriterT go

instance Binary Entry32 where
    put _ = undefined
    get = do t <- zeroTerminatedUtf8String
             o <- getWord32be
             s <- getWord32be
             return $ Entry32 t (fromIntegral o) (fromIntegral s)

instance Binary Entry64 where
    put _ = undefined
    get = do t <- zeroTerminatedUtf8String
             o <- getWord64be
             s <- getWord32be
             return $ Entry64 t (fromIntegral o) (fromIntegral s)

parse32 :: Integer -> LBS.ByteString -> Map.Map Text (Integer, Integer)
parse32 size input =
    let go = do
         r <- lift bytesRead
         when (r < fromIntegral size) $ do
            e <- lift get
            tell [e] >> go

        toPair (Entry32 w o s) = 
            (w, (o, s))
    in
    Map.fromList $ map toPair $ runGet (execWriterT go) input

parse64 :: Integer -> LBS.ByteString -> Map.Map Text (Integer, Integer)
parse64 size input =
    let go = do
         r <- lift bytesRead
         when (r < fromIntegral size) $ do
            e <- lift get
            tell [e] >> go

        toPair (Entry64 w o s) = 
            (w, (o, s))
    in
    Map.fromList $ map toPair $ runGet (execWriterT go) input
