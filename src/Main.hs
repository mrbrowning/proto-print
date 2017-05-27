module Main where

import Control.Monad.State
import Data.Traversable (forM)
import System.Environment (getArgs)

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser, parseFromFile)


type Identifier = String
type Value = String
data Object = Object Identifier [Item] deriving Show
data Pair = Pair Identifier Value deriving Show
data Item = ObjectItem Object | PairItem Pair deriving Show


concat3 :: [a] -> [a] -> [a] -> [a]
concat3 xs ys zs = xs ++ ys ++ zs

-- <identifier> ~ [a-z_]+
identifier :: Parser Identifier
identifier = many1 $ lower <|> char '_'

-- <value> ::= <numberValue>|<stringValue>|<enumValue>
value :: Parser Value
value = stringValue <|> numberValue <|> enumValue <|> booleanValue

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
    (Object <$> (identifier <* spaces <* char '{') <* spaces <*> many item) <*
    spaces <* char '}' <* spaces

-- <numberValue> ~ [0-9]+(\.[0-9]+(E[0-9]+)?)?
numberValue :: Parser String
numberValue =
    concat3 <$>
        many1 digit <*>
        (((++) <$> (pure <$> char '.') <*> many1 digit) <|> string "") <*>
        (((++) <$> (pure <$> char 'E') <*> many1 digit) <|> string "")

-- <stringValue> ~ "[^"]+"
stringValue :: Parser String
stringValue =
    concat3 <$>
        (pure <$> char '"') <*>
        (concat <$> (many (try (string "\\\"") <|> (pure <$> noneOf ['"'])))) <*>
        (pure <$> char '"')

-- <enumValue> ~ [A-Z_]+
enumValue :: Parser String
enumValue = many1 $ upper <|> char '_'

-- <booleanValue> ::= true|false
booleanValue :: Parser String
booleanValue = string "true" <|> string "false"

printItems :: [Item] -> State String String
printItems items = do
    indent <- get
    itemStrings <- forM items ((fmap $ \s -> indent ++ s ++ "\n") . prettyPrint)
    return $ concat itemStrings

prettyPrint :: Item -> State String String
prettyPrint (PairItem (Pair k v)) = return $ k ++ ": " ++ v
prettyPrint (ObjectItem (Object id items)) = do
    firstLine <- return $ id ++ " {\n"
    indent <- get
    put $ indent ++ "    "
    contents <- printItems items
    put indent
    return $ firstLine ++ contents ++ indent ++ "}"

main :: IO ()
main = do
    (fileName:_) <- getArgs
    result <- parseFromFile (many item) fileName
    case result of
        Left error -> putStrLn $ show error
        Right output -> putStr $ fst $ runState (printItems output) ""
