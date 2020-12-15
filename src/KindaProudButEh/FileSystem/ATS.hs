module ATS where

import Rose
import Cp
import FileSystem
import System.Directory
import System.IO
import Data.List
import Data.Maybe
import Data.Text (pack, unpack, replace)
import Data.List.Split (splitOn)
import qualified System.IO.Strict as SIO
import GHC.IO.Encoding

getDepth = cutMaybe . helper . splitOn "\n" <$.> SIO.readFile
    where cutMaybe = cond isNothing (const 0) ((+1) . fromJust)
          helper = length . filter ('.' ==) <$.> find (isPrefixOf "package")

depthify = cataFRoseM $ fix . ((rsplit getDepthIfJava id) -|- handle_l) where
    getDepthIfJava = cond (isSuffixOf ".java") (SIO.run . getDepth) (return . const 100) -- Ignore files that are not .java (sad but practical, there could potentially be workarounds)
    handle_l = (swap >< id) . assocl . (id >< split (aux . map (undistr . (id -|- assocr) . outFRose)) id) 
    aux = cond null (const 100) (flip (-) 1 . minimum . map p1)
    fix = inFRose <$.> either (fmap i1) (return . i2)

restore = safe_cleanup <$.> depthify .! importFS where 
    safe_cleanup = cond (null . p2) (cleanup . p1) (cleanup . head . p2) . split id converge
    converge (Single x) = []; converge f@(FRNode (d, dn) fs) = if d == -1 then [f] else concat $ map converge fs
    cleanup = cataFRose (inFRose . (p2 -|- (p2 >< id))) 

ats_mainop :: DirName -> (DirName, FS) -> [String]
ats_mainop dest (src, fs) = scriptify $ bmap (split id chroot) chroot fs where
    chroot = unpack . replace (pack src) (pack dest) . pack 
    scriptify = cataFRose $ either write_cp write_mkdir
    write_cp (x,y) = ["cp \"" ++ x ++ "\" \"" ++ y ++ "\""]
    write_mkdir (x,s) = ["mkdir -p \"" ++ x ++ "\""] ++ concat s 

ats_mainop_forall root maven = map (split (\x -> maven ++ x ++ "\\src\\main\\java") (root ++)) <$> listDirectory root >>= mapM (uncurry mainop) where
    mainop dest = ats_mainop dest . split get_root id <$.> restore
    get_root (FRNode root _) = root

call = do
    content <- ats_mainop_forall "E:\\UM\\MestradoNotGit\\work_ats\\projectsPOO_1920\\" "E:\\UM\\MestradoNotGit\\work_ats\\maven\\" 
    setLocaleEncoding utf8
    writeFile "script.sh" ("#!/bin/bash\n" ++ (concatMap (++ "\n") $ concat content))