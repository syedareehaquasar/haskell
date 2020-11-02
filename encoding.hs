import Data.List

encode :: Eq b => [b] -> [(Int, b)]
encode xs = map (\x -> (length x,head x)) (group xs)

main = print (encode "abhfjsafjhajkhahbhjhdnkjbakjdf")