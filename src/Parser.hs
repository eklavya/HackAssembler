{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Parser where

import           Addressing
import           Control.Applicative    ((*>), (<$), (<$>), (<*), (<*>))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Char              (intToDigit, isDigit, isLetter)
import           Data.Either
import           Data.List              (foldl')
import           Data.Map.Strict        as DMS
import           Data.Maybe
import           Data.Set               as DS
import           Source
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.String     (Parser)
import           Text.Parsec.Token
import           Translate



sym :: (Monad m) => ParsecT String a m Command
sym = Symbol <$> (try (skipMany endOfLine *> spaces *> char '(' <* spaces) *> many (alphaNum <|> oneOf "_.$:")) <* (spaces *> char ')' <* spaces <* skipMany comment)


ref :: (Monad m) => ParsecT String a m Term
ref = Sym <$> (((:) <$> letter <*> many (alphaNum <|> oneOf "_.$:")) <* spaces <* skipMany comment)


constant :: (Monad m) => ParsecT String a m Term
constant = do
  i <- getInput
  -- trace ("constant called on " ++ i) $ pure ()
  Constant . read <$> (many digit <* spaces <* skipMany comment)


comment :: (Monad m) => ParsecT String a m Command
comment = Comment <$> (try (skipMany endOfLine *> spaces *> string "//") *> many (noneOf "\n\r"))


aInst :: (Monad m) => ParsecT String a m Command
aInst = AInst <$> (try (skipMany endOfLine *> spaces *> char '@') *> choice [ref, constant] <* skipMany comment)


comp :: (Monad m) => ParsecT String a m String
comp = skipMany endOfLine *> spaces *> choice [
  try $ string "D+1",
  try $ string "A+1",
  try $ string "D-1",
  try $ string "A-1",
  try $ string "D+A",
  try $ string "D-A",
  try $ string "A-D",
  try $ string "D&A",
  try $ string "D|A",
  try $ string "M+1",
  try $ string "M-1",
  try $ string "D+M",
  try $ string "D-M",
  try $ string "M-D",
  try $ string "D&M",
  try $ string "D|M",
  try $ string "0",
  try $ string "1",
  try $ string "-1",
  try $ string "D",
  try $ string "A",
  try $ string "!D",
  try $ string "!A",
  try $ string "M",
  try $ string "!M",
  try $ string "-M"
              ]


dest :: (Monad m) => ParsecT String a m String
dest = skipMany endOfLine *> spaces *> choice [
  try $ string "AMD",
  try $ string "MD",
  try $ string "AM",
  try $ string "AD",
  try $ string "M",
  try $ string "D",
  try $ string "A"
              ] <* char '='


jump :: (Monad m) => ParsecT String a m String
jump = option "" $ choice [
  try $ string "JGT",
  try $ string "JEQ",
  try $ string "JGE",
  try $ string "JLT",
  try $ string "JNE",
  try $ string "JLE",
  try $ string "JMP"
              ]


cInst :: (Monad m) => ParsecT String a m Command
cInst = (CInst <$> try dest <*> comp <*> jump) <|>  (CInst <$> pure "" <*> try comp <*> (char ';' *> jump))


ign :: (Monad m) => ParsecT String a m Command
ign = do
    ((spaces *> skipMany endOfLine <* spaces) <|> (endOfLine *> spaces) <|> skipMany (char '\r')) *> eof
    return Ign


runPs :: (Monad m, MonadError String m) => String -> ParsecT String (Set String, Set String) m ((Set String, Set String), [Command])
runPs fname = do
    i <- getInput
    ls <- pure $ lines i
    cs <- forM ls $ runParserT (choice [cInst, aInst, comment, sym, ign]) () fname
    forM_ (zip [1..] cs) $ \case

        (ind, Left e) -> do
          let psp = errorPos e
          let newError = Text.Parsec.Error.setErrorPos (setSourceLine psp ind) e
          throwError $ show newError

        (_, Right (Symbol n)) -> do
          (syms, vars) <- getState
          if DS.member n syms
            then throwError $ "error: multiple definitions for symbol " ++ n
            else setState (DS.insert n syms, vars)

        (_, Right (AInst (Sym v))) -> do
              (syms, vars) <- getState
              setState (syms, DS.insert v vars)

        _ -> pure ()

    (syms, vars) <- getState
    let st = (syms, DS.difference (DS.difference vars (keysSet defSyms)) syms)
    return (st, Prelude.filter isCode $ rights cs)



