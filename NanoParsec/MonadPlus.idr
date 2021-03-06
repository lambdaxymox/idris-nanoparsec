module NanoParsec.MonadPlus


export
interface (Monad m) => MonadPlus (m : Type -> Type) where
    mzero : m a
    mplus : m a -> m a -> m a
