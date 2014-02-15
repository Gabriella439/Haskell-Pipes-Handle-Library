{-| This module is designed to be imported qualified:

> import qualified Pipes.Read as R
-}

module Pipes.Read (
    -- * Read-only Handles
    -- $readOnly
      repeatM

    -- * Read-only Transformations
    -- $transform
    , map
    , mapM
    , filter
    , filterM
    , sequence
    , chain
    , read
    , show

    -- * Utilities
    , zip
    , zipWith

    -- * Streaming
    -- $stream
    , stream
    ) where

import Control.Applicative (Applicative, liftA2)
import Pipes
import Prelude hiding (map, mapM, filter, sequence, read, show, zip, zipWith)
import qualified Prelude

{- $readOnly
    @pipes@ models read-only handles as values of type:

> Monad m => Effect' m a

    The above read-only handle is an action that runs some effect in a base
    monad @m@ and produces a value of type \'@a@\'.

    Defining a read-only handle is usually as simple as 'lift'ing an action from
    the base monad.  For example, 'repeatM' is more verbose than its
    implementation:

> repeatM :: m a -> Effect' m a
> repeatM = lift

    To read from a handle, use 'runEffect':

>>> runEffect (repeatM getLine)
Test<Enter>
Test
>>>

    This module provides only a single primitive read-only handle, since handles
    that may terminate with EOF are more idiomatically modeled by 'Producer's
    (see 'Pipes.Prelude.stdinLn' for an example of this idiom).  You may
    optionally choose to use the @io-streams@ convention of using 'Maybe' to
    denote EOF:

> stdin :: Effect' IO (Maybe String)
> stdin = lift $ do
>     eof <- isEOF
>     if eof
>         then return Nothing
>         else fmap Just getLine

    However, @pipes@ does not officially support this idiom.
-}

{-| Create a read-only handle that endlessly repeats the same read action

> Pipes.Prelude.repeatM m = stream (Pipes.Read.repeatM m)
-}
repeatM :: Monad m => m a -> Effect' m a
repeatM = lift
{-# INLINABLE repeatM #-}

{- $transform
    You can transform read-only handles to produce new output types by composing
    transformations downstream of them.  @pipes@ models these read-only
    transformations as values of type:

> Monad m => Consumer' a m b

    The above transformation 'await's an \'@a@\' each time it wishes to read
    from upstream and returns a new output of type \'@b@\'.  For example, here
    is how 'filter' is defined:

> filter :: Monad m => (a -> Bool) -> Consumer' a m a
> filter predicate = do
>     a <- await
>     if (predicate a)
>         then return a
>         else filter predicate

    'filter' uses 'await' to read from upstream as many times as necessary until
    the predicate is satisfied and the return value provides the final output.
    Transformations may 'await' multiple times to read more than once from
    upstream.

    You compose transformations downstream of handles using ('>~'):

> (>~) :: Effect     m a
>      -> Consumer a m b
>      -> Effect     m b

    For example, you can create a new read-only handle that only emits non-empty
    lines of input:

> import Pipes
> import qualified Pipes.Read as R
>
> notNull :: Effect' IO String
> notNull = R.repeatM getLine >~ R.filter (not . null)

    This generates a new read-only handle, which you can read from the same way
    as a primitive handle, using 'runEffect':

>>> runEffect notNull
<Enter>
Test<enter>
"Test"
>>>

    You can compose transformations, too, using the same ('>~') operator:

> (>~) :: Consumer a m b
>      -> Consumer b m c
>      -> Consumer a m c

    It doesn't matter what order you compose transformations or handles:

> import Data.Char (toUpper)
>
> read1 :: Effect' IO String
> read1 = (R.repeatM getLine >~ R.filter (not . null)) >~ R.map (map toUpper)
>
> read2 :: Effect' IO String
> read2 = R.repeatM getLine >~ (R.filter (not . null) >~ R.map (map toUpper))

    They will always behave identically because ('>~') is associative:

>>> runEffect read1
<Enter>
Test<Enter>
"TEST"
>>> runEffect read2
<Enter>
Test<Enter>
"TEST"

    Therefore you can omit the parentheses since the behavior is unambiguous:

> read = R.repeatM getLine >~ R.filter (not . null) >~ R.map (map toUpper)

    Also, 'await' is the identity transformation which auto-forwards all
    read requests further upstream:

> await >~ f = f
>
> f >~ await = f

    Therefore, ('>~') and 'await' form the category of read-only handles and
    their transformations, where ('>~') is the associative composition operator
    and 'await' is the identity morphism.
-}

{-| Transform a read-only handle using a function

> Pipes.Prelude.map f = stream (Pipes.Read.map f)
-}
map :: Monad m => (a -> b) -> Consumer' a m b
map f = fmap f await
{-# INLINABLE map #-}

{-| Transform a read-only handle using a monadic function

> Pipes.Prelude.mapM f = stream (Pipes.Read.mapM f)
-}
mapM :: Monad m => (a -> m b) -> Consumer' a m b
mapM f = do
    a <- await
    lift (f a)
{-# INLINABLE mapM #-}

{-| Transform a read-only handle to only output elements that satisfy a
    predicate

> Pipes.Prelude.filter f = stream (Pipes.Read.filter f)
-}
filter :: Monad m => (a -> Bool) -> Consumer' a m a
filter predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then return a
            else go
{-# INLINABLE filter #-}

{-| Transform a read-only handle to only output elements that satisfy a
    monadic predicate

> Pipes.Prelude.filter f = stream (Pipes.Read.filter f)
-}
filterM :: Monad m => (a -> m Bool) -> Consumer' a m a
filterM predicate = go
  where
    go = do
        a <- await
        keep <- lift (predicate a)
        if keep
            then return a
            else go
{-# INLINABLE filterM #-}

{-| Transform a read-only handle to output the result of a monadic action

> Pipes.Prelude.sequence = stream Pipes.Read.sequence
-}
sequence :: Monad m => Consumer' (m a) m a
sequence = do
    m <- await
    lift m
{-# INLINABLE sequence #-}

{-| Transform a read-only handle by running an action right before returning the
    result:

> Pipes.Prelude.chain f = stream (Pipes.Read.chain f)
-}
chain :: Monad m => (a -> m ()) -> Consumer' a m a
chain f = do
    a <- await
    lift (f a)
    return a
{-# INLINABLE chain #-}

{-| Transform a read-only handle to parse values using 'Read'

    Parse failures are discarded

> Pipes.Prelude.read = stream Pipes.Read.read
-}
read :: (Monad m, Read a) => Consumer' String m a
read = go
  where
    go = do
        str <- await
        case (reads str) of
            [(a, "")] -> return a
            _         -> go
{-# INLINABLE read #-}

{-| Transform a read-only handle to 'Show' all outputs

> Pipes.Prelude.show = stream Pipes.Read.show
-}
show :: (Monad m, Show a) => Consumer' a m String
show = map Prelude.show
{-# INLINABLE show #-}

{-| Zip two read-only handles or two transformations:

> zip :: Monad m => Effect'     m a -> Effect'     m b -> Effect'     m (a, b)
>
> zip :: Monad m => Consumer' x m a -> Consumer' x m b -> Consumer' x m (a, b)
-}
zip :: Applicative f => f a -> f b -> f (a, b)
zip = liftA2 (,)
{-# INLINABLE zip #-}

{-| Zip two read-only handles or two transformations using the given function:

> zipWith
>     :: Monad m
>     => (a -> b -> c) -> Effect'     m a -> Effect'     m b -> Effect'     m c
>
> zipWith
>     :: Monad m
>     => (a -> b -> c) -> Consumer' x m a -> Consumer' x m b -> Consumer' x m c
-}
zipWith :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
zipWith = liftA2
{-# INLINABLE zipWith #-}

{- $stream
    "Pipes.Read" idioms are 100% compatible with @pipes@ idioms.  Use 'stream'
    to upgrade all read-only handles or transformations into their equivalent
    @pipes@ idioms.

    Note that you can also read directly from handles using ('>~') instead of
    using 'stream':

> stream f >-> p = f >~ p
-}

{-| Convert a read-only handle into a 'Producer':

> stream :: Effect' m a -> Producer a m r

    ... or a read-only transformation into a 'Pipe':

> stream :: Consumer' a m b -> Pipe a b m r

    'stream' defines a functor that maps the category of reads to the category
    of pull-based pipes:

> stream (f >~ g) = stream f >-> stream g
>
> stream await = cat
-}
stream :: Monad m => Proxy x' x () b m b -> Proxy x' x () b m r
stream m = m >~ cat
{-# INLINABLE stream #-}
