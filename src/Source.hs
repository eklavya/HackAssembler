module Source where

data Command = Symbol String | AInst Term | CInst String String String | Comment String | Address Int | Ign
  deriving(Eq, Show, Ord)


isSymbol :: Command -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False


isCode :: Command -> Bool
isCode Ign         = False
isCode (Comment _) = False
isCode _           = True


data Term = Sym String | Constant Int
  deriving(Eq, Show, Ord)
