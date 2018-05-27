module Day3

countFromNat : Nat -> Stream Nat
countFromNat k = k :: countFromNat (k + 1)

firstn : Nat -> Stream Nat -> List Nat
firstn Z xs = []
firstn (S k) (value :: xs) = value :: firstn k xs

-- firstn 10 (countFromNat 1)
-- take 10 (countFromNat 1)

hi : IO ()
hi = putStrLn (show 42)

echo : IO ()
echo = getLine >>= putStrLn

namespace NotTotol

  loopy : IO ()
  loopy
    = do putStr "what is your name ? "
         name <- getLine
         putStrLn ("hi " ++ name)
         loopy

namespace Totol

  %default total

  data Command : Type -> Type where
    PutStr : String -> Command ()
    GetStr : Command String

  %name Command cmd

  data InfIO : Type where
    Do : Command a -> (a -> Inf InfIO) -> InfIO

  (>>=) : Command a -> (a -> Inf InfIO) -> InfIO
  (>>=) = Do

  loopy : InfIO
  loopy
    = do PutStr "what is your name ? "
         name <- GetStr
         PutStr ("hi " ++ name ++ "\n")
         loopy

  runCommand : Command a -> IO a
  runCommand (PutStr x) = putStr x
  runCommand GetStr = getLine

  partial
  run : InfIO -> IO ()
  run (Do cmd f)
    = do res <- runCommand cmd
         run (f res)

  data Fuel = Dry | More (Lazy Fuel)

  tank : Nat -> Fuel
  tank Z = Dry
  tank (S k) = More (tank k)

  run_total : Fuel -> InfIO -> IO ()
  run_total Dry y = putStrLn "out of fuel!"
  run_total (More x) (Do cmd f)
    = do res <- runCommand cmd
         run_total x (f res)

  partial
  forever : Fuel
  forever = More forever
