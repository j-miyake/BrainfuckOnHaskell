import Control.Monad.State
import Data.Char
import System.Exit

data BFState = BFState { ptr :: Int
                       , mem :: [Int]
                       , prgPtr :: Int
                       , prg :: String
                       } deriving Show

{-初期状態を構成する-}
initialState = BFState { ptr = 0
                       , mem = replicate 256 0
                       , prgPtr = 0
                       , prg = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>."
                            ++ ">++.+++++++..+++.>-.------------.<++++++++."
                            ++ "--------.+++.------.--------.>+."
                       }

main :: IO ()
main = roop ((), initialState)

roop :: ((), BFState) -> IO b
roop st = do
    s <- runStateT modState $ snd st
    roop s

modState :: StateT BFState IO ()
modState = do
    s <- get
    if length (prg s) -1 < prgPtr s then liftIO exitSuccess else
      (case prg s !! prgPtr s of
                     '+' -> get >>= \s -> put (incr s)
                     '-' -> get >>= \s -> put (decr s)
                     '.' -> get >>= \s -> lift $ putChar $ chr (mem s !! ptr s)
                     ',' -> do char <- liftIO getChar; get >>= \s -> put (ipt s (ord char))
                     '>' -> get >>= \s -> put (forward s)
                     '<' -> get >>= \s -> put (back s)
                     '[' -> get >>= \s -> put (bl s)
                     ']' -> get >>= \s -> put (br s)
                     otherwise -> lift $ print "undefined operation"
                     )
    get >>= \s -> put (prgForward s)
    {-get >>= lift.print-} -- "メモリ"の状態を見たいときにはずす

{-ポインタをインクリメントする-}
incr :: BFState -> BFState
incr s = let heads = take (ptr s) (mem s)
             (repl:tails) = drop (ptr s) (mem s)
         in s {mem = heads ++ (repl+1):tails}

{-ポインタをデクリメントする-}
decr :: BFState -> BFState
decr s = let heads = take (ptr s) (mem s)
             (repl:tails) = drop (ptr s) (mem s)
         in s {mem = heads ++ (repl-1):tails}

{-ポインタが指す値をインクリメントする-}
forward :: BFState -> BFState
forward s = s {ptr = ptr s + 1}

{-ポインタが指す値をデクリメントする-}
back :: BFState -> BFState
back s = s {ptr = ptr s - 1}

{-1バイトを入力してポインタが指す値に代入する-}
ipt:: BFState -> Int -> BFState
ipt s c = let heads = take (ptr s) (mem s)
              (repl:tails) = drop (ptr s) (mem s)
          in s {mem = heads ++ c:tails}

{-プログラムをひとつ進める-}
prgForward :: BFState -> BFState
prgForward s = s {prgPtr = prgPtr s + 1}

{-プログラムをひとつ戻す-}
prgBack :: BFState -> BFState
prgBack s = s {prgPtr = prgPtr s - 1}

{-ポインタが指す値が0なら、対応する ] の直後までジャンプする-}
bl :: BFState -> BFState
bl s = if mem s !! ptr s == 0 then toBr s else s

toBr :: BFState -> BFState
toBr s = if prg s !! prgPtr s == ']' then s else toBr (prgForward s)

{-ポインタが指す値が0でないなら、対応する [ にジャンプする-}
br :: BFState -> BFState
br s = if mem s !! ptr s /= 0 then toBl s else s

toBl :: BFState -> BFState
toBl s = if prg s !! prgPtr s == '[' then s else toBl (prgBack s)
