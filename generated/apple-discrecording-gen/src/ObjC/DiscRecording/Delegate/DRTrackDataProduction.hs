{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol DRTrackDataProduction@.
--
-- Usage:
--
-- @
-- delegate <- newDRTrackDataProduction defaultDRTrackDataProductionOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.DiscRecording.Delegate.DRTrackDataProduction
  ( DRTrackDataProductionOverrides(..)
  , defaultDRTrackDataProductionOverrides
  , newDRTrackDataProduction
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

-- | Overrides record for @\@protocol DRTrackDataProduction@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data DRTrackDataProductionOverrides = DRTrackDataProductionOverrides
  { _estimateLengthOfTrack :: !(Maybe (RawId -> IO Int))
  , _prepareTrack_forBurn_toMedia :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _cleanupTrackAfterBurn :: !(Maybe (RawId -> IO ()))
  , _prepareTrackForVerification :: !(Maybe (RawId -> IO Bool))
  , _cleanupTrackAfterVerification :: !(Maybe (RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultDRTrackDataProductionOverrides :: DRTrackDataProductionOverrides
defaultDRTrackDataProductionOverrides = DRTrackDataProductionOverrides
  { _estimateLengthOfTrack = Nothing
  , _prepareTrack_forBurn_toMedia = Nothing
  , _cleanupTrackAfterBurn = Nothing
  , _prepareTrackForVerification = Nothing
  , _cleanupTrackAfterVerification = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE drTrackDataProductionDelegateClass #-}
drTrackDataProductionDelegateClass :: Class
drTrackDataProductionDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsDRTrackDataProduction" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_estimateLengthOfTrack = unSelector (mkSelector "estimateLengthOfTrack:")
      sel_prepareTrack_forBurn_toMedia = unSelector (mkSelector "prepareTrack:forBurn:toMedia:")
      sel_cleanupTrackAfterBurn = unSelector (mkSelector "cleanupTrackAfterBurn:")
      sel_prepareTrackForVerification = unSelector (mkSelector "prepareTrackForVerification:")
      sel_cleanupTrackAfterVerification = unSelector (mkSelector "cleanupTrackAfterVerification:")
  -- estimateLengthOfTrack:
  stub_0 <- wrap_at_Q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRTrackDataProductionOverrides
    case _estimateLengthOfTrack rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (fromIntegral r)
  addObjCMethod cls "estimateLengthOfTrack:" "Q@:@" stub_0

  -- prepareTrack:forBurn:toMedia:
  stub_1 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRTrackDataProductionOverrides
    case _prepareTrack_forBurn_toMedia rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "prepareTrack:forBurn:toMedia:" "B@:@@@" stub_1

  -- cleanupTrackAfterBurn:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRTrackDataProductionOverrides
    case _cleanupTrackAfterBurn rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cleanupTrackAfterBurn:" "v@:@" stub_2

  -- prepareTrackForVerification:
  stub_3 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRTrackDataProductionOverrides
    case _prepareTrackForVerification rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "prepareTrackForVerification:" "B@:@" stub_3

  -- cleanupTrackAfterVerification:
  stub_4 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRTrackDataProductionOverrides
    case _cleanupTrackAfterVerification rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "cleanupTrackAfterVerification:" "B@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRTrackDataProductionOverrides
    if queriedSel == sel_estimateLengthOfTrack then pure (maybe 0 (const 1) (_estimateLengthOfTrack rec_))
    else if queriedSel == sel_prepareTrack_forBurn_toMedia then pure (maybe 0 (const 1) (_prepareTrack_forBurn_toMedia rec_))
    else if queriedSel == sel_cleanupTrackAfterBurn then pure (maybe 0 (const 1) (_cleanupTrackAfterBurn rec_))
    else if queriedSel == sel_prepareTrackForVerification then pure (maybe 0 (const 1) (_prepareTrackForVerification rec_))
    else if queriedSel == sel_cleanupTrackAfterVerification then pure (maybe 0 (const 1) (_cleanupTrackAfterVerification rec_))
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
newDRTrackDataProduction :: DRTrackDataProductionOverrides -> IO RawId
newDRTrackDataProduction overrides = do
  inst <- class_createInstance drTrackDataProductionDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
