import Control.Applicative
import System.Environment
import System.IO
import SkewHeap
import Test.QuickCheck
import Data.Maybe
-- | Bids.

data Bid
  = Buy Person Price           -- Person offers to buy share
  | Sell Person Price          -- Person offers to sell share
  | NewBuy Person Price Price  -- Person changes buy bid
  | NewSell Person Price Price -- Person changes sell bid
  deriving Show

data BuyBid = BuyBid Person Price
data SellBid = SellBid Person Price


instance Show BuyBid where
  show (BuyBid n p) = n ++ " " ++ show p

instance Show SellBid where
  show (SellBid n p) = n ++ " " ++ show p 

instance Eq BuyBid where
  (==) (BuyBid _ p) (BuyBid _ p') = p == p'

instance Eq SellBid where
  (==) (SellBid _ p) (SellBid _ p') = p == p'

instance Ord SellBid where
  compare (SellBid _ p) (SellBid _ p') = compare p p'

instance Ord BuyBid where
  (<=) (BuyBid _ p) (BuyBid _ p') = p >= p'



data OrderBook = OrderBook (SkewHeap BuyBid) (SkewHeap SellBid)

instance Show OrderBook where
  show (OrderBook bs ss) = "Order book:\n" ++
                           "Sellers: " ++ show ss ++ "\n" ++
                           "Buyers: " ++ show bs  


type Person = String
type Price = Integer

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').

parseBid :: String -> Either String Bid
parseBid s = case words s of
  name : kind : prices ->
    case (kind, mapM readInteger prices) of
      ("K",  Just [price])              -> Right (Buy name price)
      ("S",  Just [price])              -> Right (Sell name price)
      ("NK", Just [oldPrice, newPrice]) -> Right (NewBuy name oldPrice newPrice)
      ("NS", Just [oldPrice, newPrice]) -> Right (NewSell name oldPrice newPrice)
      _ -> Left s
  _ -> Left s
  where
  readInteger :: String -> Maybe Integer
  readInteger s = case filter (null . snd) $ reads s of
    [(x, _)] -> Just x
    _        -> Nothing

-- | Parses a sequence of bids. Correctly formatted bids are returned
-- (in the order encountered), and an error message is printed for
-- each incorrectly formatted bid.

parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
  check (Left bid)  = do
    hPutStrLn stderr $ "Malformed bid: " ++ bid
    return []
  check (Right bid) = return [bid]

-- | The main function of the program.

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> process stdin
    [f] -> process =<< openFile f ReadMode
    _   -> hPutStr stderr $ unlines
      [ "Usage: ./Lab2 [<file>]"
      , "If no file is given, then input is read from standard input."
      ]
  where
  process h = trade =<< parseBids =<< hGetContents h

-- | The core of the program. Takes a list of bids and executes them.

trade :: [Bid] -> IO ()
trade bs = trade' bs (OrderBook Leaf Leaf)
    where
      trade' :: [Bid] -> OrderBook -> IO()
      trade' [] o     = print o
      trade' (x:xs) o@(OrderBook bs ss) = case x of
        b@(Buy _ _) -> trade' xs =<< (processTrade b o)
        s@(Sell _ _) -> trade' xs =<< (processTrade s o)
        (NewSell n op np) -> do
          (OrderBook bs ss) <- processTrade (Sell n np) o
          trade' xs (OrderBook bs (removeVal ss (SellBid n op)))
        (NewBuy n op np) -> do
          (OrderBook bs ss) <- processTrade (Buy n np) o
          trade' xs (OrderBook (removeVal bs (BuyBid n op)) ss)
        {--(NewBuy name oldPrice newPrice) -> do
                                            let ons = (trade' xs (OrderBook (removeVal bs (BuyBid name oldPrice)) (remove ss)))
                                            let onf = (trade' xs (OrderBook (insert (removeVal bs (BuyBid name oldPrice)) (BuyBid name newPrice)) ss))
                                            let b = (BuyBid name newPrice)
                                            case (extractMin ss) of
                                                  Just s -> attemptPurchase b s ons onf
                                                  Nothing -> onf 
        (NewSell name oldPrice newPrice) -> do
                                            let ons = (trade' xs (OrderBook (remove bs) (removeVal ss (SellBid name oldPrice))))
                                            let onf = (trade' xs (OrderBook bs (insert (removeVal ss (SellBid name oldPrice)) (SellBid name newPrice))))
                                            let s = (SellBid name newPrice)
                                            case (extractMin bs) of
                                                  Just b -> attemptPurchase b s ons onf
                                                  Nothing -> onf--}

printTransaction :: (Person, Person) -> Price -> IO ()
printTransaction (n, n') p = putStrLn $ n ++ " buys from " ++ n' ++ " for " ++ show p ++ "kr"
      
processTrade :: Bid -> OrderBook -> IO OrderBook
processTrade (Buy n p) (OrderBook bs ss) | isJust (extractMin ss) && p >= (price $ fromJust min) = do
                                            let (SellBid n' _) = fromJust $ min
                                            printTransaction (n, n') p  
                                            return $ OrderBook bs (remove ss)
                                         | otherwise = return $  OrderBook (insert bs (BuyBid n p)) ss
                                          where
                                            min = extractMin ss
                                            price (SellBid _ p) = p
processTrade (Sell n p) (OrderBook bs ss) | isJust (extractMin bs) && p <= (price $ fromJust min) = do
                                            let (BuyBid n' _) = fromJust $ min
                                            printTransaction (n, n') p  
                                            return $ OrderBook (remove bs) ss
                                          | otherwise = return $ OrderBook bs (insert ss (SellBid n p))
                                           where
                                             min = extractMin bs
                                             price (BuyBid _ p) = p 



--testing      
instance Arbitrary Bid where
  arbitrary = do
    name <- elements ["Retard", "Moron"]
    price <- choose(60, 120)
    result <- elements [Buy name price, Sell name price]
    return result

bidGen :: Gen [Bid]
bidGen = vectorOf 100 arbitrary

test :: IO ()
test = do
  list <- generate bidGen
  trade list