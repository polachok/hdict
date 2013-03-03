{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (catch)
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text,pack)
import qualified Data.Text.IO as Text
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Text.HTML.TagSoup (parseTags,Tag(..))
import Control.Monad
import Control.Monad.Exception
import System.Environment
import System.FilePath.Posix
import System.IO (openBinaryFile,IOMode(..),Handle)
import System.Directory (doesFileExist)
import qualified StarDict.Format.Idx as Idx
import qualified StarDict.Format.Ifo as Ifo
import qualified StarDict.Format.Dict as Dict
import Control.Applicative ((<$>))

data Dictionary = Dictionary { options  :: ![Ifo.Opt],
                               index  :: Map Text (Integer, Integer),
                               dict :: Lazy.ByteString }

doFile :: String -> IO (Either SomeException (Handle, Handle, Handle))
doFile name =
    let fn = fst $ splitExtension name
        exts = [".ifo", ".idx", ".dict"]
    in runExceptionT . ExceptionT $ flip catch (return . Left) $ do
        [x, y, z] <- mapM (flip openBinaryFile ReadMode) $ map ((++) fn) exts
        return $ Right (x, y, z)

withDictionary :: String -> (Dictionary -> IO ()) -> IO ()
withDictionary path f =
    doFile path >>= \t -> case t of
       Left err -> putStrLn (show err)
       Right (ifo, idx, dict) -> do
            ifoc <- (Ifo.parse . decodeUtf8) <$> Strict.hGetContents ifo
            case ifoc of
                Left e -> putStrLn e
                Right info -> do
                    let [ifs] = [s | Ifo.IdxFileSize s <- info]
                    index <- Idx.parse32 ifs <$> Lazy.hGetContents idx
                    dictc <- Lazy.hGetContents dict
                    f (Dictionary info index dictc)

removeTags :: Text -> Text
removeTags s = mconcat [t | TagText t <- parseTags s] 

find :: Text -> Dictionary -> IO ()
find w d =
    case Map.lookup w (index d) of
         Nothing -> Text.putStrLn (w <> " not found")
         Just at -> Text.putStrLn $ removeTags $ Dict.lookup at (dict d)

main :: IO ()
main = getArgs >>= \args ->
       case args of
            [] -> error "usage: HDict <dict> [word]"
            (dict:word:[]) ->
                 withDictionary dict $ \d -> find (pack word) d
            (dict:[]) ->
                 withDictionary dict $ \d -> forever $
                    Text.getLine >>= flip find d
