module Main where

import Data.List
import Control.Monad
import System.Directory
import Control.Monad.Extra
import Control.Arrow
import Data.List.Split hiding (split)
import Data.Maybe
import System.Environment
import System.IO
import qualified System.IO.Strict as SIO
import System.Process
import Cp
import Iso
import Control.Applicative
import GHC.IO.Encoding

type Package = String

format :: FilePath -> IO [FilePath]
format d = map (d ++) . flip (\\) [".",".."] <$> getDirectoryContents d

func :: FilePath -> IO [FilePath]
func d = do (a,b) <- partitionM doesFileExist =<< format d
            let (aa, bb) = (filter filterFunc a, map (++ "\\") b)
            case bb of [] -> return aa
                       x -> func_aux (aa,bb)
    where
        filterFunc x = (not . isPrefixOf "."  . awful $ x) && ("java" `isSuffixOf` x)
        func_aux = conc . (id >< concat) <$.> lstr . (id >< mapM func)
        awful = reverse >=< takeWhile (/= '/')

foo :: IO [(String, [FilePath])]
foo = mapM (dstr . split fullPath func) =<< dirFilesFullPath
    where strip = (++ "\\src\\main\\java\\") . reverse >=< takeWhile (/= '\\') . tail
          fullPath s = (++ strip s) <$> fmap (++ "\\projects_maven\\") getCurrentDirectory
          dirFilesFullPath = map (snoc "\\") <$> ((++ "\\projectsPOO_1920\\") <$> getCurrentDirectory >>= format)

-- saca package do ficheiro
getPkg :: FilePath -> IO Package
getPkg = (SIO.run . SIO.readFile) >=> return . cutMaybe . helper . splitOn "\n"
    where cutMaybe = cond isNothing nil fromJust
          readFileOP = flip openFile ReadMode >=> (\h -> do {hSetEncoding h latin1; hGetContents h})
          replace a b = map (\x -> if x == a then b else x)
          helper = find (isPrefixOf "package") >=> return . replace '.' '/' . tail . dropWhile (/= ' ') . (reverse >=< tail . dropWhile (/= ';'))

pkgSort :: [FilePath] -> IO [(Package, [FilePath])]
pkgSort =  fmap (map (split (p1 . head) (map p2)) . groupBy (factor (==)) . sortBy (factor compare)) . mapM (rstr . split getPkg id)
    where factor f x y = f (p1 x) (p1 y)

f (x, l) = concatMap (\(p, lf) -> (if p /= "" then "mkdir -p \"" ++ x ++ p ++ "\"" else ""):map (\fi -> "cp \"" ++ fi ++ "\" \"" ++ x ++ p ++ "\"") lf) l

allCommands = fmap (concatMap (uncurry (:) . split ((++) "mkdir -p " . p1) f)) commands

runCommands = allCommands >>= mapM callCommand

commands = foo >>= mapM (lstr . (id >< pkgSort))

main = do
  cs <- allCommands 
  setLocaleEncoding utf8
  mapM_ putStrLn cs
