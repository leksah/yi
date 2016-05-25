{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Random GTK utils
module Yi.UI.Pango.Utils where

import Control.Exception (catch, throw)

import Data.Text (append)
import Paths_yi
import System.FilePath
import GI.GdkPixbuf.Objects.Pixbuf
       (pixbufAddAlpha, pixbufNewFromFile, Pixbuf(..))
import Data.GI.Base
       (gerrorNew, gerrorMessage, gerrorCode, gerrorDomain, GError)
import qualified Data.Text as T (pack)

loadIcon :: FilePath -> IO Pixbuf
loadIcon fpath = do
  iconfile <- getDataFileName $ "art" </> fpath
  icoProject <-
    catch (pixbufNewFromFile $ T.pack iconfile)
    (\(e :: GError) -> do
      dom  <- gerrorDomain e
      code <- gerrorCode e
      msg  <- gerrorMessage e
      throw =<< gerrorNew dom code (
        msg `append` " -- use the yi_datadir environment variable to"
            `append` " specify an alternate location"))
  pixbufAddAlpha icoProject True 0 255 0
