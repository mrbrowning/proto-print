module Main where

import Control.Monad.State
import Data.Foldable (foldl')
import System.Environment (getArgs)

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser, parseFromFile)


type Identifier = String
type Value = String
data Object = Object Identifier [Item] deriving Show
data Pair = Pair Identifier Value deriving Show
data Item = ObjectItem Object | PairItem Pair deriving Show


-- for cases where type inference might not be able to infer what return means
toList :: a -> [a]
toList = return

concat3 :: [a] -> [a] -> [a] -> [a]
concat3 xs ys zs = xs ++ ys ++ zs

-- <identifier> ~ [a-z_]+
identifier :: Parser Identifier
identifier = many1 $ lower <|> char '_'

-- <pair> ::= <identifier>: <value>
pair :: Parser Pair
pair = spaces *> (Pair <$> (identifier <* char ':' <* space) <*> value) <* spaces

-- <item> ::= <object>|<pair>
item :: Parser Item
item = try (ObjectItem <$> object) <|> (PairItem <$> pair)

-- <object> ::= <identifier> { <item-list> }
-- <item-list> ::= <item>|<item><item-list>
object :: Parser Object
object =
    spaces *>
    (Object <$> (identifier <* spaces <* char '{') <*> many1 item) <*
    char '}' <* spaces

-- <value> ~ ("[^"]+"|[0-9]+\.[0-9]+|[A-Z_]+)
value :: Parser Value
value =
    (concat3 <$>
        (fmap toList $ char '"') <*>
        (many $ noneOf ['"']) <*>
        (fmap toList $ char '"')) <|>
    (concat3 <$> many1 digit <*> (fmap toList $ char '.') <*> many1 digit) <|>
    (many1 $ upper <|> char '_')

prettyPrint :: Item -> State String String
prettyPrint (PairItem (Pair k v)) = return $ k ++ ": " ++ v
prettyPrint (ObjectItem (Object id items)) = do
    firstLine <- return $ id ++ " {\n"
    indent <- get
    put $ indent ++ "    "
    contents <- printItems items
    put indent
    return $ firstLine ++ (foldl' (++) [] contents) ++ indent ++ "}"
  where
    printItems :: [Item] -> State String [String]
    printItems items = do
        indent <- get
        sequence $ map ((fmap (\s -> indent ++ s ++ "\n")) . prettyPrint) items

main :: IO ()
main = do
    (fileName:_) <- getArgs
    result <- parseFromFile item fileName
    case result of
        Left _ -> putStrLn "Couldn't parse protobuf"
        Right item -> putStrLn $ fst $ runState (prettyPrint item) ""
