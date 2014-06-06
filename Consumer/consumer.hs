{-# LANGUAGE DeriveFunctor #-}

module Consumer where
	import Probabilities hiding (BST)
	import Data.Array.IArray
	import System.Random
	import Data.List (maximumBy)
	import Data.Ord (comparing)

	fix f = let x = f x in x

	{--data BST a = Tree { left :: BST a, value :: a, right :: BST a} | Empty
		deriving (Eq, Show, Read, Functor)

	insert Empty x = Tree Empty x Empty
	insert (Tree l y r) x	| x < y		= Tree (insert l x) y r
							| otherwise	= Tree l y (insert r x)--}

	randomUtility :: (RandomGen r, Monad m) => Int -> Int -> DistributionT r m ((Array (Int, Int) Float))
	randomUtility sizex sizey = do
		seed <- Distribution return
		let knot arr = runDist (utilities arr) seed
		let uArray = fix knot :: Array (Int, Int) Float
		return uArray
			where
				xbounds = [0 .. (sizex - 1)]
				ybounds = [0 .. (sizey - 1)]
				utilities arr = do
					xs <- (sequence . (invertX $ [utility (x, y) | x <- xbounds, y <- ybounds])) arr
					return (listArray ((0, 0), (sizex - 1, sizey - 1)) xs)
				utility p arr = case p of
					(0, 0) -> return 0.0
					(x, y) -> do
						-- increasing returns (monotonically increasing for all x and y)
						let left = if x > 0 then arr ! (x - 1, y) else 0.0
						let up = if y > 0 then arr ! (x, y - 1) else 0.0
						let lowerbound = max left up
						-- diminishing marginal utility (convexity for all x and y)
						let twoleft = if x > 1 then Just (arr ! (x - 2, y)) else Nothing
						let twoup = if y > 1 then Just (arr ! (x, y - 2)) else Nothing
						let upperX = twoleft >>= \tl -> return (2 * left - tl)
						let upperY = twoup >>= \tu -> return (2 * up - tu)
						let upperbound = min (maybe (1000.0) id upperX) (maybe (1000.0) id upperY)
						-- return a uniformly distributed value that satisfies the two constraints
						uniform lowerbound upperbound
				invertX xs = \u -> map ($ u) xs
				differentialArray arr = listArray ((0, 0), (sizex - 1, sizey - 1))
					[differential (x, y) arr | x <- xbounds, y <- ybounds] :: Array (Int, Int) (Float, Float)
				differential p arr = case p of
					(0, 0) -> (arr ! (1, 0), arr ! (0, 1))
					(0, y) -> (arr ! (1, y) - arr ! (0, y), arr ! (0, y) - arr ! (0, y - 1))
					(x, 0) -> (arr ! (x, 0) - arr ! (x - 1, 0), arr ! (x, 1) - arr ! (x, 0))
					(x, y) -> (arr ! (x, y) - arr ! (x - 1, y), arr ! (x, y) - arr ! (x, y - 1))

	bundles :: Float -> Float -> Float -> [(Int, Int)]
	bundles budget px py = [(x, y) | x <- [0 .. maxX], let y = f x]
		where
			maxX = floor (budget / px)
			f x = floor ((budget - (px * (fromIntegral x))) / py)

	optimalBundle :: Array (Int, Int) Float -> Int -> Int -> Float -> Float -> Float -> (Int, Int)
	optimalBundle arr sizex sizey budget px py = case possibleBundles of
		[] -> (sizex - 1, sizey - 1)
		_  -> maximumBy (comparing ((!) arr)) possibleBundles
		where
			withinArray (x, y) = (x >= 0) && (x < sizex) && (y >= 0) && (y < sizey)
			possibleBundles = case (px, py) of
				(0, 0) -> []
				(0, _) -> [(sizex - 1, floor (budget / py))]
				(_, 0) -> [(floor (budget / px), sizey - 1)]
				(_, _) -> filter withinArray (bundles budget px py)