{-# LANGUAGE LambdaCase #-}

module Translate where

import           Data.Char (intToDigit, isDigit, isLetter)
import           Numeric   (showIntAtBase)
import           Source

tDest = \case
                        "" -> "000"
                        "AMD" -> "111"
                        "MD" -> "011"
                        "AM" -> "101"
                        "AD" -> "110"
                        "M" -> "001"
                        "D" -> "010"
                        "A" -> "100"

tComp = \case
                        "D+1" -> "0011111"
                        "A+1" -> "0110111"
                        "D-1" -> "0001110"
                        "A-1" -> "0110010"
                        "D+A" -> "0000010"
                        "D-A" -> "0010011"
                        "A-D" -> "0000111"
                        "D&A" -> "0000000"
                        "D|A" -> "0010101"
                        "M+1" -> "1110111"
                        "M-1" -> "1110010"
                        "D+M" -> "1000010"
                        "D-M" -> "1010011"
                        "M-D" -> "1000111"
                        "D&M" -> "1000000"
                        "D|M" -> "1010101"
                        "0" -> "0101010"
                        "1" -> "0111111"
                        "-1" -> "0111010"
                        "D" -> "0001100"
                        "A" -> "0110000"
                        "!D" -> "0001101"
                        "!A" -> "0110001"
                        "M" -> "1110000"
                        "!M" -> "1110001"
                        "-M" -> "1110011"

tJump = \case
                        "" -> "000"
                        "JGT" -> "001"
                        "JEQ" -> "010"
                        "JGE" -> "011"
                        "JLT" -> "100"
                        "JNE" -> "101"
                        "JLE" -> "110"
                        "JMP" -> "111"


translate :: [Command] -> [String]
translate = fmap (\case
                    AInst (Constant i) ->
                      let bs = showIntAtBase 2 intToDigit i ""
                      in replicate (16 - length bs) '0' ++ bs

                    CInst dest comp jump -> "111" ++ tComp comp ++ tDest dest ++ tJump jump
                 )