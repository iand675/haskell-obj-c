{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKSavedGameListener@.
--
-- Usage:
--
-- @
-- delegate <- newGKSavedGameListener defaultGKSavedGameListenerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKSavedGameListener
  ( GKSavedGameListenerOverrides(..)
  , defaultGKSavedGameListenerOverrides
  , newGKSavedGameListener
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol GKSavedGameListener@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKSavedGameListenerOverrides = GKSavedGameListenerOverrides
  { _player_didModifySavedGame :: !(Maybe (RawId -> RawId -> IO ()))
  , _player_hasConflictingSavedGames :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKSavedGameListenerOverrides :: GKSavedGameListenerOverrides
defaultGKSavedGameListenerOverrides = GKSavedGameListenerOverrides
  { _player_didModifySavedGame = Nothing
  , _player_hasConflictingSavedGames = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkSavedGameListenerDelegateClass #-}
gkSavedGameListenerDelegateClass :: Class
gkSavedGameListenerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKSavedGameListener" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_player_didModifySavedGame = unSelector (mkSelector "player:didModifySavedGame:")
      sel_player_hasConflictingSavedGames = unSelector (mkSelector "player:hasConflictingSavedGames:")
  -- player:didModifySavedGame:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKSavedGameListenerOverrides
    case _player_didModifySavedGame rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "player:didModifySavedGame:" "v@:@@" stub_0

  -- player:hasConflictingSavedGames:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKSavedGameListenerOverrides
    case _player_hasConflictingSavedGames rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "player:hasConflictingSavedGames:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKSavedGameListenerOverrides
    if queriedSel == sel_player_didModifySavedGame then pure (maybe 0 (const 1) (_player_didModifySavedGame rec_))
    else if queriedSel == sel_player_hasConflictingSavedGames then pure (maybe 0 (const 1) (_player_hasConflictingSavedGames rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newGKSavedGameListener :: GKSavedGameListenerOverrides -> IO RawId
newGKSavedGameListener overrides = do
  inst <- class_createInstance gkSavedGameListenerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
