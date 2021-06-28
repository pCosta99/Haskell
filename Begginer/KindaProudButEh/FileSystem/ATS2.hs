module ATS2 where

import Data.List
import Cp
import List
import Iso
import Control.Monad
import System.IO
import Data.Maybe
import Data.List.Split (splitOn)
import Data.Text (unpack, pack, replace)
import FileSystem
import Rose
import GHC.IO.Encoding
import System.Directory
import qualified System.IO.Strict as SIO

getPkg :: FilePath -> IO String
getPkg = (SIO.run . SIO.readFile) >=> return . cutMaybe . helper . splitOn "\n" where
    cutMaybe = cond isNothing nil fromJust
    replaceS a b = unpack . replace (pack a) (pack b) . pack
    helper = find (isPrefixOf "package") >=> return . replaceS "." "\\" . tail . dropWhile (/= ' ') . (reverse >=< tail . dropWhile (/= ';'))

leafsToScript :: FilePath -> [FilePath] -> IO [String]
leafsToScript dest = hyloListM conquer divide where
    conquer = return . either nil (conc . ((tostr . split (\x -> "\"" ++ opt (p1 x) ++ p1 x ++ "\"") mergePkgPath) >< id))
    opt = cond null (const dest) (const $ dest ++ "\\") 
    mergePkgPath (pkg, path) = let dpkg = cond null (const "\\") (\x -> "\\" ++ pkg ++ "\\") pkg in map (split id (\x -> dest ++ dpkg ++ getName x)) path
    getName = reverse >=< takeWhile (/= '\\')
    tostr = cons . (("mkdir -p " ++) >< (map (\(x,y) -> "cp \"" ++ x ++ "\"" ++ " \"" ++ y ++ "\"")))
    divide = dl2 . cond null (i1 . bang) (i2 . f)
    f = fmap assocl . lstr . split p1 spanPkg .! rstr . (getPkg >< id) . split head id :: [FilePath] -> IO ((String, [FilePath]),[FilePath])
    spanPkg = fmap (\(x,y) -> pow2 (map p2) $ span ((==x) . p1) y) . lstr . (id >< (sequence . map (rsplit getPkg id))) -- parte a lista de ficheiros consoante façam ou não parte do pkg 

ats_mainop_forall root maven = map (split (\x -> maven ++ x ++ "\\src\\main\\java") (root ++)) <$> listDirectory root >>= mapM (uncurry mainop) where
    mainop dest = leafsToScript dest . filter (isSuffixOf ".java") . leafs .! importFS

call = do
    content <- ats_mainop_forall "E:\\UM\\MestradoNotGit\\work_ats\\projectsPOO_1920\\" "E:\\UM\\MestradoNotGit\\work_ats\\maven\\" 
    setLocaleEncoding utf8
    writeFile "script.sh" ("#!/bin/bash\n" ++ (concatMap (++ "\n") $ concat content))