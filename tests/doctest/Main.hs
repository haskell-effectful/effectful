module Main (main) where

import Control.Monad
import System.Directory
import System.FilePath
import Test.DocTest

main :: IO ()
main = do
    ms <- findModules "src"
    doctest $
        -- [ "--fast"
        [ "-XBangPatterns"
        , "-XConstraintKinds"
        , "-XDataKinds"
        , "-XFlexibleContexts"
        , "-XFlexibleInstances"
        , "-XGADTs"
        , "-XGeneralizedNewtypeDeriving"
        , "-XLambdaCase"
        , "-XMultiParamTypeClasses"
        , "-XRankNTypes"
        , "-XRecordWildCards"
        , "-XRoleAnnotations"
        , "-XScopedTypeVariables"
        , "-XStrictData"
        , "-XTupleSections"
        , "-XTypeApplications"
        , "-XTypeFamilies"
        , "-XTypeOperators"
        , "-isrc"
        ] <> ms

findModules :: FilePath -> IO [FilePath]
findModules = go [] . (:[])
    where
        go !acc [] = return acc
        go !acc (dir:todo) = do
            xs <- listDirectory dir
            let xs' = map (dir </>) xs
            ds <- filterM doesDirectoryExist xs'
            fs <- filterM doesFileExist xs'
            let ms = filter (".hs" `isExtensionOf`) fs
            go (acc <> ms) (ds <> todo)
