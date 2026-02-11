{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKGameModel@.
--
-- Usage:
--
-- @
-- delegate <- newGKGameModel defaultGKGameModelOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameplayKit.Delegate.GKGameModel
  ( GKGameModelOverrides(..)
  , defaultGKGameModelOverrides
  , newGKGameModel
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

-- | Overrides record for @\@protocol GKGameModel@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKGameModelOverrides = GKGameModelOverrides
  { _setGameModel :: !(Maybe (RawId -> IO ()))
  , _gameModelUpdatesForPlayer :: !(Maybe (RawId -> IO RawId))
  , _applyGameModelUpdate :: !(Maybe (RawId -> IO ()))
  , _scoreForPlayer :: !(Maybe (RawId -> IO Int))
  , _isWinForPlayer :: !(Maybe (RawId -> IO Bool))
  , _isLossForPlayer :: !(Maybe (RawId -> IO Bool))
  , _unapplyGameModelUpdate :: !(Maybe (RawId -> IO ()))
  , _players :: !(Maybe (IO RawId))
  , _activePlayer :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGKGameModelOverrides :: GKGameModelOverrides
defaultGKGameModelOverrides = GKGameModelOverrides
  { _setGameModel = Nothing
  , _gameModelUpdatesForPlayer = Nothing
  , _applyGameModelUpdate = Nothing
  , _scoreForPlayer = Nothing
  , _isWinForPlayer = Nothing
  , _isLossForPlayer = Nothing
  , _unapplyGameModelUpdate = Nothing
  , _players = Nothing
  , _activePlayer = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkGameModelDelegateClass #-}
gkGameModelDelegateClass :: Class
gkGameModelDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKGameModel" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setGameModel = unSelector (mkSelector "setGameModel:")
      sel_gameModelUpdatesForPlayer = unSelector (mkSelector "gameModelUpdatesForPlayer:")
      sel_applyGameModelUpdate = unSelector (mkSelector "applyGameModelUpdate:")
      sel_scoreForPlayer = unSelector (mkSelector "scoreForPlayer:")
      sel_isWinForPlayer = unSelector (mkSelector "isWinForPlayer:")
      sel_isLossForPlayer = unSelector (mkSelector "isLossForPlayer:")
      sel_unapplyGameModelUpdate = unSelector (mkSelector "unapplyGameModelUpdate:")
      sel_players = unSelector (mkSelector "players")
      sel_activePlayer = unSelector (mkSelector "activePlayer")
  -- setGameModel:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    case _setGameModel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setGameModel:" "v@:@" stub_0

  -- gameModelUpdatesForPlayer:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    case _gameModelUpdatesForPlayer rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "gameModelUpdatesForPlayer:" "@@:@" stub_1

  -- applyGameModelUpdate:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    case _applyGameModelUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applyGameModelUpdate:" "v@:@" stub_2

  -- scoreForPlayer:
  stub_3 <- wrap_at_q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    case _scoreForPlayer rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (fromIntegral r)
  addObjCMethod cls "scoreForPlayer:" "q@:@" stub_3

  -- isWinForPlayer:
  stub_4 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    case _isWinForPlayer rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "isWinForPlayer:" "B@:@" stub_4

  -- isLossForPlayer:
  stub_5 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    case _isLossForPlayer rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "isLossForPlayer:" "B@:@" stub_5

  -- unapplyGameModelUpdate:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    case _unapplyGameModelUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "unapplyGameModelUpdate:" "v@:@" stub_6

  -- players
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    case _players rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "players" "@@:" stub_7

  -- activePlayer
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    case _activePlayer rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "activePlayer" "@@:" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelOverrides
    if queriedSel == sel_setGameModel then pure (maybe 0 (const 1) (_setGameModel rec_))
    else if queriedSel == sel_gameModelUpdatesForPlayer then pure (maybe 0 (const 1) (_gameModelUpdatesForPlayer rec_))
    else if queriedSel == sel_applyGameModelUpdate then pure (maybe 0 (const 1) (_applyGameModelUpdate rec_))
    else if queriedSel == sel_scoreForPlayer then pure (maybe 0 (const 1) (_scoreForPlayer rec_))
    else if queriedSel == sel_isWinForPlayer then pure (maybe 0 (const 1) (_isWinForPlayer rec_))
    else if queriedSel == sel_isLossForPlayer then pure (maybe 0 (const 1) (_isLossForPlayer rec_))
    else if queriedSel == sel_unapplyGameModelUpdate then pure (maybe 0 (const 1) (_unapplyGameModelUpdate rec_))
    else if queriedSel == sel_players then pure (maybe 0 (const 1) (_players rec_))
    else if queriedSel == sel_activePlayer then pure (maybe 0 (const 1) (_activePlayer rec_))
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
newGKGameModel :: GKGameModelOverrides -> IO RawId
newGKGameModel overrides = do
  inst <- class_createInstance gkGameModelDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
