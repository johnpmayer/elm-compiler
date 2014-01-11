
{-# OPTIONS_GHC -W #-}

module Language.Elm.Compile where

import Control.Monad (foldM)
import qualified Data.Maybe as Maybe
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory
import System.FilePath

import Build.Dependencies (getSortedDependencies)
import qualified Generate.Html as Html
import qualified Metadata.Prelude as Prelude
import qualified Build.Utils as Utils
import qualified Build.Flags as Flag
import qualified Build.File as File
import qualified Elm.Internal.Paths as Path

import qualified System.IO.Temp as Temp

buildAll :: [(String, String)] -> String -> IO String
buildAll modules rootFile = do
  --Make the temp directory to do the compilation
  --cd <- getCurrentDirectory
  --putStrLn $ " current dir " ++ cd
  --TODO remove
  Temp.withTempDirectory "" ".elm_temp" (\dir -> 
    let
        flags = Flag.flags
        appendToOutput :: BS.ByteString -> FilePath -> IO BS.ByteString
        appendToOutput js filePath = do
          src <- BS.readFile (Utils.elmo flags filePath)
          return (BS.append src js)
          
        sources js = map Html.Link (Flag.scripts flags) ++ [ Html.Source js ]
          
        makeHtml js moduleName = ("html", BS.pack $ renderHtml html)
            where
              rtsPath = Maybe.fromMaybe Path.runtime (Flag.runtime flags)
              html = Html.generate rtsPath (takeBaseName rootFile) (sources js) moduleName ""
    in do
       --TODO remove
       mapM (uncurry writeFile) modules
       cd <- getCurrentDirectory
       setCurrentDirectory dir
       
       --Copy all the files to the temp folder
       mapM (uncurry writeFile) modules
       
       
       let noPrelude = Flag.no_prelude flags
       builtIns <- Prelude.interfaces noPrelude

       files <- if Flag.make flags
                then getSortedDependencies (Flag.src_dir flags) builtIns rootFile
                else return [rootFile]

       (moduleName, _) <-
           File.build flags (length files) builtIns "" files

       js <- foldM appendToOutput BS.empty files

       (extension, code) <-
           if Flag.only_js flags
           then do putStr "Generating JavaScript ... "
                   return ("js", js)
           else do putStr "Generating HTML ... "
                   return (makeHtml js moduleName)

       let targetFile = Utils.buildPath flags rootFile extension
       createDirectoryIfMissing True (takeDirectory targetFile)
       BS.writeFile targetFile code
       putStrLn "Done"

       setCurrentDirectory cd

       return $ show code
       )
       
  

          
