{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text,pack)
import qualified Data.Text.IO as Text
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Text.HTML.TagSoup (parseTags,Tag(..))
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
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

openBinaryFile' :: String -> IOMode -> IO (Maybe Handle)
openBinaryFile' s m =
    doesFileExist s >>= \b ->
    if b
       then liftM Just (openBinaryFile s m)
       else return Nothing

doFile :: String -> IO (Maybe (Handle, Handle, Handle))
doFile fn =
    let fn' = fst $ splitExtension fn
        exts = ["ifo", "idx", "dict"]
        open n e = let n' = addExtension n e in
                   MaybeT . WriterT $ do
                        f <- openBinaryFile' n' ReadMode
                        runWriterT $ do
                            tell ["Opening " ++ n']
                            return f

    in (runWriterT $ runMaybeT $ mapM (open fn') exts) >>= \r ->
      case r of
        (Nothing, log) ->
             (putStrLn $ last log ++ " failed") >> return Nothing
        (Just (x:y:z:[]), _) ->
             return $ Just (x,y,z)

withDictionary :: String -> (Dictionary -> IO ()) -> IO ()
withDictionary path f =
    doFile path >>= \t -> case t of
       Nothing -> return ()
       Just (x, y, z) -> do
            info <- (Ifo.parse . decodeUtf8) <$> Strict.hGetContents x
            case info of 
                Left e -> putStrLn e
                Right info' -> do
                    let [ifs] = [s | Ifo.IdxFileSize s <- info']
                    idx <- Idx.parse32 ifs <$> Lazy.hGetContents y
                    dict <- Lazy.hGetContents z
                    f (Dictionary info' idx dict)

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
