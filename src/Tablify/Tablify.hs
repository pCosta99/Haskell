module Tablify (TableInfo, createTableInfo, tablify, printT) where

import Text.PrettyPrint.Boxes as T
import Data.List
import List
import Cp

type Matrix a = [[a]]

data Padding = LeftP | CenterP | RightP deriving (Show, Eq)

data TableInfo = TI { info :: Matrix String, -- the info to display
                        cell_length :: [Int], -- maximum length of each column
                        cell_alignment :: Alignment, -- alignment, suggests we dont need the padding ???
                        lines :: Int, -- number of lines of the matrix
                        columns :: Int, -- number of columns of the matrix
                        v_sep_char :: Char, -- vertical
                        h_sep_char :: Char, -- horizontal
                        c_sep_char :: Char, -- corner
                        padding :: Padding -- padding
                      } deriving Show

{--------------------------------------------------------- PADDING FUNCTIONS ---------------------------------------------------------
    Padding functions. Each one has a one to one mapping to a certain padding type.
-}
-- Left padding
pad_left width x = x ++ replicate k ' ' where 
    k = width - length x

-- Right padding
pad_right width x =  replicate k ' ' ++ x where 
    k = width - length x

-- Center padding
pad_center :: Int -> String -> String
pad_center width x = replicate (p1 k') ' ' ++ x ++ replicate (p2 k') ' ' where 
    k = width - length x
    k' = if even k then (div k 2,div k 2) else (div (k-1) 2, div (k+1) 2) 

-- Gives a padding function according to the padding type on the table info given
getPadding = cond ((==) LeftP) (const pad_left) (cond ((==) CenterP) (const pad_center) (const pad_right)) . padding

{--------------------------------------------------------- HELPER FUNCTIONS ----------------------------------------------------------
    Functions with specific purposes that were needed more than one time and, therefore, got abstracted
-}

-- Wraps a list with something
wrap = uncurry (++) . swap . split (singl . p1) cons
-- Wraps a list with something and intersperse's it too
wrap_n_fill = wrap . split p1 (uncurry intersperse)

{------------------------------------------------------ CONTROLLING FUNCTIONS --------------------------------------------------------
    Functions to work with TableInfo. 
    Some are public some are not.
    Tablify is the function that turns the TableInfo structure into a box to be displayed.
-}

-- Transforms a TableInfo into the correct displayable table
tablify :: TableInfo -> Box
tablify ti = vcat left $ wrap_n_fill (horizontal_sep ti, get_data_rdy ti) where
    ((cs, hs),vs) = ((singl $ c_sep_char ti, h_sep_char ti),singl $ v_sep_char ti) -- separators
    c_length = cell_length ti 
    horizontal_sep = hcat left . map T.text . curry wrap_n_fill cs . map (flip replicate hs) . cell_length
    get_data_rdy = map (hcat left . map T.text . curry wrap_n_fill vs . map (uncurry (getPadding ti)) . zip c_length) . info

-- Sets the text padding as left 
t_pad_left ti = ti { padding = LeftP }    
-- Sets the text padding as center
t_pad_center ti = ti { padding = CenterP }
-- Sets the text padding as right
t_pad_right ti = ti { padding = RightP }

-- Sets the vertical separator as the symbol given
set_v_sep sep ti = ti { v_sep_char = sep }
-- Sets the horizontal separator as the symbol given
set_h_sep sep ti = ti { h_sep_char = sep }
-- Sets the corner separator as the symbol given
set_c_sep sep ti = ti { c_sep_char = sep }

{--------------------------------------------------------- MAIN FUNCTIONS ------------------------------------------------------------
    Main functions of the module.
    createTableInfo allows the end-user to load a certain matrix into a TableInfo.
    He can then manipulate with certain functions and display it with tablify whenever he feels like.
-}

-- Main function of the module. For a given Matrix of A it creates a structure that can be used to print the matrix in tabular format
createTableInfo :: (Show a) => [[a]] -> TableInfo                      
createTableInfo m = TI info cell_length left lines columns '|' '-' '+' CenterP where
    info = map (map show) m
    cell_length = map maximum $ transpose $ map (map length) info
    lines = length info
    columns = length (info !! 0)

-- Prints a certain table
printT = putStrLn . render . tablify

{--------------------------------------------------------- TESTING AREA --------------------------------------------------------------
    Section with testing features.
-}

-- Some input just for testing purposes
-- Will eventually be switched into maybe a generator
input :: TableInfo
input = createTableInfo [["Products", "Amount of sales", "Profit"],
                         ["AF1184","150","2000"],
                         ["AF1184","150","2000"],
                         ["AF1184","150","2000"],
                         ["AF1184","150","2000"],
                         ["AF1184","150","2000"],
                         ["AF1184","150","2000"]]