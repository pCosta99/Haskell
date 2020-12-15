module FileSystem where

import Cp
import Rose
import System.Directory
import Data.List

-- In this module we use the Rose algebra to manipulate interaction with the file system.
-- As a first approach my main concern is to be able to import a certain FS and query it through catamorphisms.
-- As a later objective, one may try to create actual OS operations one the imported FS.

type FS = FRose FileName DirName

type FileName = String
type DirName = String

-- Receives the root that we want to import and creates the FS accordingly.
-- This root can either be a file or a directory.
importFS :: FilePath -> IO FS
importFS = anaFRoseM importFS_gene

-- Keep the gene out since it is useful to create hylo operations.
importFS_gene :: FilePath -> IO (Either FilePath (DirName, [FilePath]))
importFS_gene = after_grd .! m_grd doesFileExist where
    after_grd = dl . (id -|- (propagate <$.> lsplit id listDirectory))
    propagate (p, fps) = (p,(++) (p ++ "\\") <$> fps)
    dl = either (return . i1) (fmap i2)

-- Well, after importing a FS, the sky is the limit really. 
-- We can define a grep-like function in a somewhat simple way, like this.
grep :: String -> FilePath -> IO [FileName]
grep s = hyloFRoseM grep_gene importFS_gene where
    grep_gene = either (cond (isInfixOf s . p1) (singl . p2) nil <$.> rsplit readFile id) (return . concat . p2)