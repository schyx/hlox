module Parser (Parser (..)) where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Bifunctor (Bifunctor (second))

newtype Parser dat output = Parser {runParser :: dat -> Maybe (dat, output)}

instance Functor (Parser dat) where
  fmap f p = Parser $ fmap (second f) . runParser p

instance Applicative (Parser dat) where
  pure x = Parser $ \inData -> Just (inData, x)
  (Parser pf) <*> (Parser px) =
    Parser $ \inData -> do
      (midData, f) <- pf inData
      (endData, x) <- px midData
      Just (endData, f x)

instance Alternative (Parser dat) where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \inData -> p1 inData <|> p2 inData

instance Monad (Parser dat) where
  return = pure
  (Parser p) >>= f = Parser $ \inData ->
    case p inData of
      Nothing -> Nothing
      Just (midData, firstVal) -> runParser (f firstVal) midData
