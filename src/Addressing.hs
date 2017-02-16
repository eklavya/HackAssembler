{-# LANGUAGE LambdaCase #-}

module Addressing where

import           Control.Applicative
import           Data.List
import           Data.Map.Strict     as DMS
import           Data.Maybe
import           Data.Set            as DS
-- import           Debug.Trace
import           Source


defSyms :: DMS.Map String Int
defSyms = DMS.fromList [
  ("R0", 0),
  ("R1", 1),
  ("R2", 2),
  ("R3", 3),
  ("R4", 4),
  ("R5", 5),
  ("R6", 6),
  ("R7", 7),
  ("R8", 8),
  ("R9", 9),
  ("R10", 10),
  ("R11", 11),
  ("R12", 12),
  ("R13", 13),
  ("R14", 14),
  ("R15", 15),
  ("SP", 0),
  ("LCL", 1),
  ("ARG", 2),
  ("THIS", 3),
  ("THAT", 4),
  ("SCREEN", 16384),
  ("KBD", 24576)

                      ]



addressify :: ((Set String, Set String), [Command]) -> [Command]
addressify ((syms, vars), code) = do
  let varMap = DMS.fromList ((\ (v, i) -> (v, i + 16)) <$> zip (DS.toList vars) [0 ..])
  let symMap = DMS.union (fst $ Data.List.foldl' (\(m, iCount) b -> case b of
                                    -- keep an instruction count and increment it only for AInst or CInst
                                    -- when we see a symbol we create an entry for the symbol and the current instruction number
                                    Symbol s -> (DMS.insert s iCount m, iCount)
                                    _        -> (m, iCount + 1)) (DMS.empty, 0) code) defSyms
  let noSymbols = Prelude.filter (not . isSymbol) code
  fmap (\case
           (AInst (Sym s)) -> AInst $ Constant $ fromJust $ DMS.lookup s symMap <|> DMS.lookup s varMap
           x             -> x) noSymbols
