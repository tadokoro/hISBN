{- |
   Module        : ISBN
   Copyright     : Copyright (C) 2009 Hidekazu Tadokoro, Tokyo Institute of Technology, Japan
   License       : GPL

   Maintainer    : Hidekazu Tadokoro (tadokoro@csg.is.titech.ac.jp)
   stability     : experimental
   Portability   : portable

   ISBN utility functions
   Especially stuff for converting between ISBN-10 and ISBN-13
   This program is originated from http://www.staff.ncl.ac.uk/d.j.wilkinson/software/isbn.py
   thanks Darren J Wilkinson !
-}

module ISBN (isbn_strip, convert, isValid, check, isI10, isI13, url) where
import Text.Regex
import Data.Char (digitToInt)
import Data.List (isPrefixOf)

-- Strip whitespace, hyphens, etc. from an ISBN number and return the result.
isbn_strip isbn = let short = subRegex (mkRegex "\\W") isbn "" in
                  subRegex (mkRegex "\\D") short "X"

-- Convert an ISBN-10 to ISBN-13 or vice-versa.
convert isbn = let short = isbn_strip isbn in
               if not $ isValid short then
                   error "Invalid ISBN"
               else
                   if length short == 10 then
                       let stem = "978" ++ dropTail 1 short in
                       stem ++ check stem
                   else 
                       if isPrefixOf "978" short then
                           let stem = drop 3 $ dropTail 1 short in
                           stem ++ check stem
                       else
                           error "ISBN not convertible"
    where 
      dropTail n xs = reverse $ drop n $ reverse xs

--  Check the validity of an ISBN. Works for either ISBN-10 or ISBN-13.
isValid isbn = let short = isbn_strip isbn in
               case length short of
                 10 -> isI10 short
                 13 -> isI13 short
                 _ -> False

-- Compute the check digit for the stem of an ISBN. Works with either
-- the first 9 digits of an ISBN-10 or the first 12 digits of an ISBN-13.
check stem = let short = isbn_strip stem in
             case length short of
               9 -> checkI10 short
               12 -> checkI13 short
               _ -> ""

-- Computes the ISBN-10 check digit based on the first 9 digits of a
-- stripped ISBN-10 number.
checkI10 stem = let sum' = sum [digit * digitToInt c | (c, digit) <- zip stem [10,9..]]
                    check = 11 - (sum' `mod` 11) in
                case check of
                  10 -> "X"
                  11 -> "0"
                  _ -> show check

-- Checks the validity of an ISBN-10 number.
isI10 isbn = let short = isbn_strip isbn in
             if length short /= 10 then
                 False
             else
                 let sum' = sum [digit * if c == 'X' || c == 'x' then 10 else digitToInt c| (c, digit) <- zip short [10,9..]] in
                     sum' `mod` 11 == 0
                 
-- Compute the ISBN-13 check digit based on the first 12 digits of a
-- stripped ISBN-13 number. 
checkI13 stem  = let sum' = sum [digitToInt c * if count `mod` 2 == 0 then 1 else 3 | (c, count) <- zip stem [0..]] in
                 show $ sum' - 10

-- Checks the validity of an ISBN-13 number.
isI13 isbn = let short = isbn_strip isbn in
             if length short /= 13 then
                 False
             else
                 let sum' = sum [digitToInt c * if count `mod` 2 == 0 then 1 else 3 | (c, count) <- zip short [0..]] in
                 sum' `mod` 10 == 0

-- Converts supplied ISBN (either ISBN-10 or ISBN-13) to a stripped
-- ISBN-10.
toI10 = toI isI10

-- Converts supplied ISBN (either ISBN-10 or ISBN-13) to a stripped
-- ISBN-13.
toI13 = toI isI13

-- Converts supplied ISBN (either ISBN-10 or ISBN-13) to a stripped
-- Implementation
toI p isbn = if not $ isValid isbn then
                 error "Invalid ISBN"
             else
                 if p isbn then
                     isbn_strip isbn
                 else
                     convert isbn

-- Returns a URL for a book, corresponding to the "type" and the "isbn"
-- provided. This function is likely to go out-of-date quickly, and is
-- provided mainly as an example of a potential use-case for the module.
-- Currently allowed types are "google-books" (the default if the type is
-- not recognised), "amazon", "amazon-uk", "blackwells".
url t isbn = let short = toI10 isbn in
             case t of
               "amazon" -> "http://www.amazon.com/o/ASIN/" ++ short
               "amazon-jp" -> "http://www.amazon.co.jp/o/ASIN/" ++ short
               "amazon-uk" -> "http://www.amazon.uk/o/ASIN/" ++ short
               "blackwells" -> 
                    "http://bookshop.blackwell.co.uk/jsp/welcome.jsp?action=search&type=isbn&term=" ++ short
               _ -> "http://books.google.com/books?vid=" ++ short
