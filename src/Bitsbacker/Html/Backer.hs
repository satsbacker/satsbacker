{-# LANGUAGE OverloadedStrings #-}

module Bitsbacker.Html.Backer where

import Lucid

home :: Html ()
home = p_ [] "hello"
