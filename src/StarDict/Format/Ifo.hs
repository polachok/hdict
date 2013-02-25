{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module StarDict.Format.Ifo(parse,Opt(..)) where
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Control.Applicative
import Data.Attoparsec.Text hiding (parse)
import Prelude hiding (unlines)
import StarDict.Format.TH

type IFO = [Opt]

data ContentType = Pure | Locale | Pango
 		    | Phonetic | Xdxf | Kana | PowerWord 
	  	    | MediaWiki | Html | ResourseList
		    | WavFile | PicFile | Experimental
	deriving (Show,Eq,Ord)

data Opt = BookName Text
         | WordCount Integer
         | SynWordCount Integer
         | IdxFileSize Integer
         | IdxOffsetBits Integer
         | Author Text
         | Email Text
         | Website Text
         | Description Text
         | Date Text
         | SameTypeSequence (Set ContentType)
    deriving (Show,Eq,Ord)

buildIFOParser ''Opt

header = string "StarDict's dict ifo file" *> skipSpace
version = string "version=" *> (string "2.4.2" <|> string "3.0.0") *> skipSpace
options = some (parseIFO <|> parseSameTypeSequence)

parseSameTypeSequence = 
    let types =
         [('m',Pure),
         ('l',Locale),
         ('g',Pango),
         ('t',Phonetic),
         ('x',Xdxf),
         ('y',Kana),
         ('k',PowerWord),
         ('w',MediaWiki),
         ('h',Html),
         ('r',ResourseList),
         ('W',WavFile),
         ('P',PicFile),
         ('X',Experimental)]
    in fmap (SameTypeSequence . Set.fromList)
       (string "sametypesequence" *> char '=' *> 
        some (anyChar >>= maybe (fail "foo") return . flip lookup types)
        <* (endOfLine <|> endOfInput))

parse :: Text -> Either String [Opt]
parse t = case parseOnly (header *> version *> options) t of
            Right x -> return x
            Left e -> fail e
