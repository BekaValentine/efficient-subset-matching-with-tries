import Data.List (foldl')
import qualified Data.Map as M

data Trie k a = Node (Maybe a) (M.Map k (Trie k a))
  deriving (Show)

simpleTrie :: Ord k => [k] -> a -> Trie k a
simpleTrie []     a = Node (Just a) M.empty
simpleTrie (k:ks) a = Node Nothing (M.fromList [(k, simpleTrie ks a)])


merge :: Ord k => [Trie k a] -> Trie k a
merge = foldl' aux (Node Nothing M.empty)
  where
    aux (Node ma ns) (Node ma' ns')
      = Node (maybe ma' Just ma)
             (M.unionWith aux ns ns')

follow :: Ord k => [k] -> Trie k a -> [(a, [k])]
follow ks t = aux [] ks t
  where
    aux ps [] (Node Nothing  _) = []
    aux ps [] (Node (Just a) _) = [(a,reverse ps)]
    aux ps ks (Node ma ns)
      = M.foldrWithKey
          (\k t r -> r ++
            case dropWhile (< k) ks of
              [] -> []
              k':ks'
                | k == k'   -> aux (k:ps) ks' t
                | otherwise -> [])
          (case ma of { Nothing -> [] ; Just a -> [(a,ps)] })
          ns
