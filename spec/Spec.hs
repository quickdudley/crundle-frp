import Reactive.Crundle
import Control.Monad

main = return ()

fourchars :: IO [Char -> IO ()]
fourchars = do
  l <- replicateM 4 sourceEvent
  bl <- mapM (\(e,_) -> stepper e const ' ') l
  let
    b = sequenceA bl
    e' = b <@ mconcat (map fst l)
  subscribe e' putStrLn
  return $ map snd l
