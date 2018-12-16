{-# LANGUAGE TupleSections #-}

module Bitsbacker.Templates where

import Control.Arrow (left)
import Control.Exception (try, SomeException)
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseError)
import Text.Mustache

import qualified Data.Text as T
import qualified Data.Text.IO as T

data TemplateType = UserTemplate
  deriving (Show, Eq, Ord, Enum)


data BBTemplate = BBTemplate {
      bbTemplateType :: TemplateType
    , bbTemplate     :: Template
    }
    deriving Show

data TemplateError = ReadError String
                   | ParseErr (ParseError Char Void)
                   deriving Show

templateName :: TemplateType -> PName
templateName tt = PName $
    case tt of
      UserTemplate -> "user"


loadTemplate :: TemplateType -> IO (Either TemplateError BBTemplate)
loadTemplate typ = do
  let pname    = templateName typ
      filename = "templates/" ++ T.unpack (unPName pname) ++ ".html"
  dat <- try (T.readFile filename) :: IO (Either SomeException Text)
  let res = do d <- left (ReadError . show) dat
               bimap ParseErr (BBTemplate typ) (compileMustacheText pname d)
  return res


templateTypes :: [TemplateType]
templateTypes = enumFrom (toEnum 0)


loadTemplates :: IO (Either [(TemplateType, TemplateError)] [BBTemplate])
loadTemplates = do
  es <- zip templateTypes <$> traverse loadTemplate templateTypes
  let es1 = map (\(tt, e) -> left (tt,) e) es
      es2 = partitionEithers es1
  return $
    case (es2) of
      ([], templates) -> Right templates
      (errs, _)       -> Left errs
