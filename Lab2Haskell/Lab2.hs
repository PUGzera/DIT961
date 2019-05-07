import Control.Applicative
import System.Environment
import System.IO
import SkewHeap
import Test.QuickCheck

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
  (>=) (BuyBid _ p) (BuyBid _ p') = p <= p'
  (>) (BuyBid _ p) (BuyBid _ p') = p < p'
  (<=) (BuyBid _ p) (BuyBid _ p') = p >= p'
  (<) (BuyBid _ p) (BuyBid _ p') = p > p'



data OrderBook = OrderBook (SkewHeap BuyBid) (SkewHeap SellBid)

instance Show OrderBook where
  show (OrderBook Leaf Leaf) = "Order book:\nSellers:\nBuyers:"
  show (OrderBook bs Leaf)   = "Order book:\n" ++
                               "Sellers:\n" ++
                               "Buyers: " ++ show bs
  show (OrderBook Leaf ss)   = "Order book:\n" ++
                               "Sellers: " ++ show ss ++ "\n" ++
                               "Buyers:"
  show (OrderBook bs ss)     = "Order book:\n" ++
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
trade bs = trade' (OrderBook Leaf Leaf) bs

trade' :: OrderBook -> [Bid] -> IO()
trade' (OrderBook Leaf Leaf) [] = print "No transactions found"
trade' orderbook []             = transaction orderbook (OrderBook Leaf Leaf)
trade' (OrderBook Leaf Leaf) (x:xs) = case x of
  (Buy name price)  -> trade' (OrderBook (insert Leaf (BuyBid name price)) Leaf) xs
  (Sell name price) -> trade' (OrderBook Leaf (insert Leaf (SellBid name price))) xs
trade' (OrderBook Leaf ss) (x:xs) = case x of
  (Buy name price)  -> do
                        let s = extractMin ss
                        let b = (BuyBid name price)
                        attemptPurchase b s (trade' (OrderBook Leaf (remove ss)) xs) (trade' (OrderBook (insert Leaf (BuyBid name price)) ss) xs)
  (Sell name price) -> trade' (OrderBook Leaf (insert ss (SellBid name price))) xs
  (NewSell name oldPrice newPrice) -> trade' (OrderBook Leaf (insert (removeVal ss (SellBid name oldPrice)) (SellBid name newPrice))) xs  

trade' (OrderBook bs Leaf) (x:xs) = case x of
  (Buy name price)  -> trade' (OrderBook (insert bs (BuyBid name price)) Leaf) xs
  (Sell name price) -> do
                        let b = extractMin bs
                        let s = (SellBid name price)
                        attemptPurchase b s (trade' (OrderBook (remove bs) Leaf) xs) (trade' (OrderBook bs (insert Leaf (SellBid name price))) xs)
  (NewBuy name oldPrice newPrice)  -> trade' (OrderBook (insert (removeVal bs (BuyBid name oldPrice)) (BuyBid name newPrice)) Leaf) xs
trade' (OrderBook bs ss) (x:xs) = case x of
  (Buy name price) -> do
                      let s = extractMin ss
                      let b = (BuyBid name price)
                      attemptPurchase b s (trade' (OrderBook bs (remove ss)) xs) (trade' (OrderBook (insert bs (BuyBid name price)) ss) xs)   
  (Sell name price) -> do
                       let b = extractMin bs
                       let s = (SellBid name price)
                       attemptPurchase b s (trade' (OrderBook (remove bs) ss) xs) (trade' (OrderBook bs (insert ss (SellBid name price))) xs)                      
  (NewBuy name oldPrice newPrice) -> do
                                      let s = extractMin ss
                                      let b = (BuyBid name newPrice)
                                      attemptPurchase b s (trade' (OrderBook (removeVal bs (BuyBid name oldPrice)) (remove ss)) xs) (trade' (OrderBook (insert (removeVal bs (BuyBid name oldPrice)) (BuyBid name newPrice)) ss) xs) 
  (NewSell name oldPrice newPrice) -> do
                                       let b = extractMin bs
                                       let s = (SellBid name newPrice) 
                                       attemptPurchase b s (trade' (OrderBook (remove bs) (removeVal ss (SellBid name oldPrice))) xs) (trade' (OrderBook bs (insert (removeVal ss (SellBid name oldPrice)) (SellBid name newPrice))) xs)   
                                                              


attemptPurchase :: BuyBid -> SellBid -> IO() -> IO() -> IO()
attemptPurchase (BuyBid name p) (SellBid name' p') onSuccess onFailure = do
                                                                          if p >= p' then do
                                                                            putStrLn $ name ++ " buys from " ++ name' ++ " for " ++ show p ++ "kr"
                                                                            onSuccess
                                                                          else do     
                                                                            onFailure

{--trade' :: OrderBook -> [Bid] -> IO ()
trade' (OrderBook Leaf Leaf) [] = print "No transactions found"
trade' orderbook []             = transaction orderbook (OrderBook Leaf Leaf)
trade' (OrderBook bs ss) (x:xs) = 
  case x of
    (Buy name price)  -> trade' (OrderBook (insert bs (BuyBid name price)) ss) xs
    (Sell name price) -> trade' (OrderBook bs (insert ss (SellBid name price))) xs
    (NewBuy name oldPrice newPrice)  -> trade' (OrderBook (insert (removeVal bs (BuyBid name oldPrice)) (BuyBid name newPrice)) ss) xs
    (NewSell name oldPrice newPrice) -> trade' (OrderBook bs (insert (removeVal ss (SellBid name oldPrice)) (SellBid name newPrice))) xs --}

transaction :: OrderBook -> OrderBook -> IO()
transaction (OrderBook bs Leaf) r@(OrderBook bs' ss) = print (OrderBook (merge bs bs') ss)
transaction (OrderBook Leaf ss) r@(OrderBook bs ss') = print (OrderBook bs (merge ss ss'))
transaction (OrderBook bs ss) o@(OrderBook bs' ss')  = do
  let (BuyBid name price)    = extractMin bs
  let (SellBid name' price') = extractMin ss
  if price >= price' then do
    print $ name ++ " buys from " ++ name' ++ " for " ++ show price ++ "kr"
    transaction (OrderBook (remove bs) (remove ss)) o
  else do
    transaction (OrderBook (remove bs) (remove ss)) (OrderBook (insert bs' (extractMin bs)) (insert ss' (extractMin ss))) 

instance Arbitrary Bid where
  arbitrary = do
    name <- elements ["Ada", "David"]
    price <- choose(60, 120)
    result <- elements [Buy name price, Sell name price]
    return result

bidGen :: Gen [Bid]
bidGen = vectorOf 100 arbitrary

test :: IO ()
test = do
  list <- generate bidGen
  trade list