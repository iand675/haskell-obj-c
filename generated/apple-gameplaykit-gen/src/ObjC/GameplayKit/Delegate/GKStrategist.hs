{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKStrategist@.
--
-- Usage:
--
-- @
-- delegate <- newGKStrategist defaultGKStrategistOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameplayKit.Delegate.GKStrategist
  ( GKStrategistOverrides(..)
  , defaultGKStrategistOverrides
  , newGKStrategist
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

-- | Overrides record for @\@protocol GKStrategist@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKStrategistOverrides = GKStrategistOverrides
  { _bestMoveForActivePlayer :: !(Maybe (IO RawId))
  , _gameModel :: !(Maybe (IO RawId))
  , _setGameModel :: !(Maybe (RawId -> IO ()))
  , _randomSource :: !(Maybe (IO RawId))
  , _setRandomSource :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKStrategistOverrides :: GKStrategistOverrides
defaultGKStrategistOverrides = GKStrategistOverrides
  { _bestMoveForActivePlayer = Nothing
  , _gameModel = Nothing
  , _setGameModel = Nothing
  , _randomSource = Nothing
  , _setRandomSource = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkStrategistDelegateClass #-}
gkStrategistDelegateClass :: Class
gkStrategistDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKStrategist" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_bestMoveForActivePlayer = unSelector (mkSelector "bestMoveForActivePlayer")
      sel_gameModel = unSelector (mkSelector "gameModel")
      sel_setGameModel = unSelector (mkSelector "setGameModel:")
      sel_randomSource = unSelector (mkSelector "randomSource")
      sel_setRandomSource = unSelector (mkSelector "setRandomSource:")
  -- bestMoveForActivePlayer
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKStrategistOverrides
    case _bestMoveForActivePlayer rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "bestMoveForActivePlayer" "@@:" stub_0

  -- gameModel
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKStrategistOverrides
    case _gameModel rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "gameModel" "@@:" stub_1

  -- setGameModel:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKStrategistOverrides
    case _setGameModel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setGameModel:" "v@:@" stub_2

  -- randomSource
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKStrategistOverrides
    case _randomSource rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "randomSource" "@@:" stub_3

  -- setRandomSource:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKStrategistOverrides
    case _setRandomSource rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setRandomSource:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKStrategistOverrides
    if queriedSel == sel_bestMoveForActivePlayer then pure (maybe 0 (const 1) (_bestMoveForActivePlayer rec_))
    else if queriedSel == sel_gameModel then pure (maybe 0 (const 1) (_gameModel rec_))
    else if queriedSel == sel_setGameModel then pure (maybe 0 (const 1) (_setGameModel rec_))
    else if queriedSel == sel_randomSource then pure (maybe 0 (const 1) (_randomSource rec_))
    else if queriedSel == sel_setRandomSource then pure (maybe 0 (const 1) (_setRandomSource rec_))
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
newGKStrategist :: GKStrategistOverrides -> IO RawId
newGKStrategist overrides = do
  inst <- class_createInstance gkStrategistDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
