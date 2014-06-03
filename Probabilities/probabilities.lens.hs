module Probabilities.Lens where
	import Prelude hiding ((.), id)
	import Control.Category
	import Control.Monad.Trans
	import Control.Monad.Trans.State.Lazy
	import Control.Monad.Identity

	data Lens a b = Lens (a -> b) (a -> b -> a)

	onlyFst = Lens fst (\(x, y) x' -> (x', y))
	onlySnd = Lens snd (\(x, y) y' -> (x, y'))
	newState state = Lens (const state) (\original state' -> original)
	newFst state = Lens (\original -> (state, original)) (\original (state', original') -> original')
	newSnd state = Lens (\original -> (original, state)) (\original (original', state') -> original')

	instance Category Lens where
		id = Lens id const
		(.) (Lens split1 merge1) (Lens split2 merge2) = Lens (split1 . split2) (\state part -> merge2 state (merge1 (split2 state) part))

	with :: Monad m => Lens s s' -> StateT s' m a -> StateT s m a
	with (Lens split merge) f = do
		state <- get
		result <- lift $ runStateT f (split state)
		put (merge state (snd result))
		return (fst result)