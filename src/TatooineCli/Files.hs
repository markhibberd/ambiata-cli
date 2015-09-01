{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TatooineCli.Files (
    ignoreNoFile
) where

import           P

import           System.IO
import           System.IO.Error

import           Control.Exception


-- If a file is there great, if not, don't care just in that case.
ignoreNoFile :: IO () -> IO ()
ignoreNoFile action =
  action `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = pure ()
          | otherwise = throwIO e
