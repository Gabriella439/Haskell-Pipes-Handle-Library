{-| This module is designed to be imported qualified:

> import qualified Pipes.Write as W
-}

{-# LANGUAGE RankNTypes #-}

module Pipes.Write (
    -- * Write-only handles
    -- $writeonly
      mapM_
    , toHandle
    , drain
    , print

    -- * Write-only transformations
    -- $transform
    , map
    , mapM
    , mapFoldable
    , concat
    , filter
    , filterM
    , sequence
    , chain
    , read
    , show

    -- * Utilities
    , tee

    -- * Streaming
    -- $stream
    , stream
    ) where

import Control.Monad (when, void)
import Pipes
import qualified System.IO as IO
import Prelude hiding
    ( concat
    , filter
    , map
    , mapM
    , mapM_
    , print
    , read
    , sequence
    , show
    )
import qualified Prelude

{- $writeonly
    @pipes@ models write-only handles as values of type:

> Monad m => c -> Effect' m ()

    The above write-only handle is an action that consumes a value of type @c@
    and runs some effect in a base monad @m@.

    Defining a write-only handle is usually as simple as 'lift'ing an action
    from the base monad.  For example, 'print' is defined like this:

> print :: Show a => a -> Effect' IO ()
> print a = lift (Prelude.print a)

    To write to a handle, supply the handle with a value and call 'runEffect':

>>> import Pipes
>>> import qualified Pipes.Write as W
>>> runEffect (W.print 1)
1
>>>

-}

{-| Create a write-only handle from a monadic function

> Pipes.Prelude.mapM_ f = stream (Pipes.Write.mapM_ f)
-}
mapM_ :: Monad m => (a -> m b) -> a -> Effect' m ()
mapM_ f a = void (lift (f a))
{-# INLINABLE mapM_ #-}

{-| Create a write-only handle from a traditional 'IO.Handle'

> Pipes.Prelude.toHandle h = stream (Pipes.Write.toHandle h)
-}
toHandle :: MonadIO m => IO.Handle -> String -> Effect' m ()
toHandle handle str = liftIO (IO.hPutStrLn handle str)
{-# INLINABLE toHandle #-}

{-| A write-only handle that discards all values

> Pipes.Prelude.drain = stream Pipes.Write.drain
-}
drain :: Monad m => a -> Effect' m ()
drain = discard
{-# INLINABLE drain #-}

{-| A write-only handle that 'Prelude.print's all values

> Pipes.Prelude.print = stream Pipes.Write.print
-}
print :: (MonadIO m, Show a) => a -> Effect' m ()
print a = liftIO (Prelude.print a)
{-# INLINABLE print #-}

{- $transform
    You can transform write-only handles to accept new input types by
    precomposing transformations upstream of them.  @pipes@ models these
    write-only transformations as values of type:

> Monad m => b -> Producer' c m ()

    The above transformation accepts a new input of type @b@ and 'yield's a @c@
    each time it wishes to write to downstream.  For example, here is how
    'filter' is defined:

> filter :: Monad m => (a -> Bool) -> a -> Producer' a m ()
> filter predicate a = when (predicate a) (yield a)

    'filter' only 'yield's the element further downstream when the element
    satisfies the predicate.  Transformations may 'yield' multiple times to
    write more than once to downstream.

    You precompose transformations upstream of handles using ('~>'):

> (~>) :: (b -> Producer c m ())
>      -> (c -> Effect     m ())
>      -> (b -> Effect     m ())

    For example, you can create a new write-only handle that refuses to output
    null 'String's by pre-composing 'filter' upstream of 'toHandle':

> import Pipes
> import qualified Pipes.Write as W
>
> printPositive :: Int -> Effect' IO ()
> printPositive = W.filter (> 0) ~> W.print

    This generates a new write-only handle, which you can write to the same way
    as a primitive handle, using 'runEffect':

>>> runEffect $ printPositive 10
10
>>> runEffect $ printPositive (-1)
>>> -- Notice how 'filter' did not forward the string to 'print'

    You can compose transformations, too, using the same ('~>') operator:

> (~>) :: (a -> Producer b m ())
>      -> (b -> Producer c m ())
>      -> (a -> Producer c m ())

    It doesn't matter what order you compose transformations or handles:

> write1 :: Int -> Effect' IO ()
> write1 = (W.map negate ~> W.filter (> 0)) ~> W.print
>
> write2 :: Int -> Effect' IO ()
> write2 = W.map negate ~> (W.filter (> 0) ~> W.print)

    They will always behave identically because ('~>') is associative:

>>> runEffect $ write1 (-5)
5
>>> runEffect $ write2 (-5)
5
>>> runEffect $ write1 5
>>> runEffect $ write2 5
>>>

    Therefore you can omit the parentheses since the behavior is unambiguous:

> write = W.map negate ~> W.filter (> 0) ~> W.print

    Also, 'yield' is the identity transformation which auto-forwards all values
    along further downstream:

> yield ~> f = f
>
> f ~> yield = f

    Therefore, ('~>') and 'yield' form the category of write-only handles and
    their transformations, where ('~>') is the associative composition operator
    and 'yield' is the identity morphism.
-}

{-| Transform a write-only handle using a function

> Pipes.Prelude.map f = stream (Pipes.Write.map f)
-}
map :: Monad m => (a -> b) -> a -> Producer' b m ()
map f a = yield (f a)
{-# INLINABLE map #-}

{-| Transform a write-only handle using a monadic function

> Pipes.Prelude.mapM f = stream (Pipes.Write.mapM f)
-}
mapM :: Monad m => (a -> m b) -> a -> Producer' b m ()
mapM f a = do
    b <- lift (f a)
    yield b
{-# INLINABLE mapM #-}

{-| Transform a write-only handle using a foldable function

> Pipes.Prelude.mapFoldable f = stream (Pipes.Write.mapFoldable f)
-}
mapFoldable :: (Monad m, Foldable t) => (a -> t b) -> a -> Producer' b m ()
mapFoldable f a = each (f a)
{-# INLINABLE mapFoldable #-}

{-| Transform a write-only handle to process individual elements of a 'Foldable'

> Pipes.Prelude.concat = stream Pipes.Write.concat
-}
concat :: (Monad m, Foldable t) => t a -> Producer' a m ()
concat = each
{-# INLINABLE concat #-}

{-| Transform a write-only handle to only process elements that satisfy a
    predicate

> Pipes.Prelude.filter f = stream (Pipes.Write.filter f)
-}
filter :: Monad m => (a -> Bool) -> a -> Producer' a m ()
filter f a = when (f a) (yield a)
{-# INLINABLE filter #-}

{-| Transform a write-only handle to only process elements that satisfy a
    monadic predicate

> Pipes.Prelude.filterM f = stream (Pipes.Write.filterM f)
-}
filterM :: Monad m => (a -> m Bool) -> a -> Producer' a m ()
filterM f a = do
    keep <- lift (f a)
    when keep (yield a)
{-# INLINABLE filterM #-}

{-| Transform a write-only handle to process the results of monadic actions

> Pipes.Prelude.sequence = stream Pipes.Write.sequence
-}
sequence :: Monad m => m a -> Producer' a m ()
sequence m = do
    a <- lift m
    yield a
{-# INLINABLE sequence #-}

{-| Transform a write-only handle by running an action before all writes

> Pipes.Prelude.chain f = stream (Pipes.Write.chain f)
-}
chain :: Monad m => (a -> m ()) -> a -> Producer' a m ()
chain f a = do
    lift (f a)
    yield a
{-# INLINABLE chain #-}

{-| Transform a write-only handle to process values parsed by 'Read'

    Parse failures are discarded

> Pipes.Prelude.read = stream Pipes.Write.read
-}
read :: (Monad m, Read a) => String -> Producer' a m ()
read str = case (reads str) of
    [(a, "")] -> yield a
    _         -> return ()
{-# INLINABLE read #-}

{-| Transform a write-only handle to process 'Show'n values

> Pipes.Prelude.show = stream Pipes.Write.show
-}
show :: (Monad m, Show a) => a -> Producer' String m ()
show = map Prelude.show
{-# INLINABLE show #-}

{-| Transform a write-only handle to a transformation that re-forwards the input
    value

> tee :: Monad m => (a -> Effect' m ()) -> (a -> Producer' a m ())
-}
tee :: Monad m => (a -> Proxy x' x () a m ()) -> a -> Proxy x' x () a m ()
tee write a = do
    write a
    yield a
{-# INLINABLE tee #-}

{- $stream
    "Pipes.Write" idioms are 100% compatible with @pipes@ idioms.  Use 'stream'
    to upgrade all write-only handles or transformations into their equivalent
    @pipes@ idioms.

    Note that you can also directly write to handles using 'for' instead of
    using 'stream':

> p >-> stream f = for p f
-}

{-| 'stream' converts write-only handles into their equivalent @pipes@
    'Consumer's:

> stream :: (a -> Effect' m ()) -> Consumer a m r

    'stream' also converts write transformations into their equivalent 'Pipe's:

> stream :: (a -> Producer' b m ()) -> Pipe a b m r

    'stream' defines a functor that maps the category of writes to the category
    of pull-based pipes:

> stream (f ~> g) = stream f >-> stream g
>
> stream yield    = cat
-}
stream :: Monad m => (a -> Proxy () a y' y m ()) -> Proxy () a y' y m r
stream = for cat
{-# INLINABLE stream #-}
