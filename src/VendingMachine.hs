module VendingMachine
    ( listItems
    , initInventory
    ) where

import Numeric

type Price = Int
type Product = (String, Price, Int)

formatFloatN :: RealFloat a => a -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

initInventory :: [Product]
initInventory =
    [("Potato Chips", 199, 10)
    ,("Pocky", 299, 10)
    ,("Granola Bar", 250, 10)]

padString :: String -> Int -> String
padString input totalLength =
    input ++ replicate needed ' '
    where needed = totalLength - length input

printPrice :: Int -> String 
printPrice price =
    "$" ++ formatFloatN (fromIntegral price / 100.0) 2

printProduct :: (Int, Product) -> IO () 
printProduct (idx, (name, price, quantity)) = do
    putStrLn (show (succ idx) ++ " " ++ (padString name 15 ++ padString (printPrice price) 4 ++ " \t " ++ show quantity ++ " left"))

processInput :: [(Int, Product)] -> IO (Int, Int)
processInput inventory = do
    ins <- getLine
    let
        inputs = words ins
    case length inputs of
        2 -> do
            let
                [p, n] = map read inputs :: [Int]
                (_, (sn, sp, sq)) = inventory !! pred p
            case (p, n) of
                (p', n')    | (p' > length inventory) || (p' < 1) -> do
                                putStrLn "You chose a nonexistent item!"
                                processInput inventory
                            | (n' > sq) || (n' < 0) -> do
                                putStrLn "Not enough in stock!"
                                processInput inventory
                _ -> return (p, n)
        _ -> do
            putStrLn "Wrong number of inputs!"
            processInput inventory
    

listItems :: [Product] -> IO ()
listItems inventory = do
    listIndexedItems $ zip [0..] inventory

listIndexedItems :: [(Int, Product)] -> IO ()
listIndexedItems indexedInventory = do
    putStrLn "Welcome to the Vending Machine!"
    putStrLn "================================="
    mapM_ printProduct indexedInventory
    putStrLn "================================="
    putStrLn "Enter the number of the product and quantity separated by a space."
    (choice, quantity) <- processInput indexedInventory
    let
        (_, selected@(sn, sp, sq)) = indexedInventory !! pred choice
    putStrLn $ "Thank you! That will be " ++ printPrice (sp * quantity) ++ "\n"
    let
        newList = map (\all@(idx, (n, p, q)) ->
            if idx == pred choice
                then (idx, (n, p, q - quantity))
                else all) indexedInventory
    -- mapM_ printProduct newList
    listIndexedItems newList

