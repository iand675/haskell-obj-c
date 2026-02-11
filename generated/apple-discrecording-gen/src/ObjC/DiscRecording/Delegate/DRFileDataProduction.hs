{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol DRFileDataProduction@.
--
-- Usage:
--
-- @
-- delegate <- newDRFileDataProduction defaultDRFileDataProductionOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.DiscRecording.Delegate.DRFileDataProduction
  ( DRFileDataProductionOverrides(..)
  , defaultDRFileDataProductionOverrides
  , newDRFileDataProduction
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

-- | Overrides record for @\@protocol DRFileDataProduction@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data DRFileDataProductionOverrides = DRFileDataProductionOverrides
  { _calculateSizeOfFile_fork_estimating :: !(Maybe (RawId -> Int -> Bool -> IO Int))
  , _prepareFileForBurn :: !(Maybe (RawId -> IO Bool))
  , _prepareFileForVerification :: !(Maybe (RawId -> IO Bool))
  , _cleanupFileAfterBurn :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultDRFileDataProductionOverrides :: DRFileDataProductionOverrides
defaultDRFileDataProductionOverrides = DRFileDataProductionOverrides
  { _calculateSizeOfFile_fork_estimating = Nothing
  , _prepareFileForBurn = Nothing
  , _prepareFileForVerification = Nothing
  , _cleanupFileAfterBurn = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_I_B_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CUInt -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CUInt -> CULong -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE drFileDataProductionDelegateClass #-}
drFileDataProductionDelegateClass :: Class
drFileDataProductionDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsDRFileDataProduction" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_calculateSizeOfFile_fork_estimating = unSelector (mkSelector "calculateSizeOfFile:fork:estimating:")
      sel_prepareFileForBurn = unSelector (mkSelector "prepareFileForBurn:")
      sel_prepareFileForVerification = unSelector (mkSelector "prepareFileForVerification:")
      sel_cleanupFileAfterBurn = unSelector (mkSelector "cleanupFileAfterBurn:")
  -- calculateSizeOfFile:fork:estimating:
  stub_0 <- wrap_at_I_B_Q $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRFileDataProductionOverrides
    case _calculateSizeOfFile_fork_estimating rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (arg2 /= 0)
        pure (fromIntegral r)
  addObjCMethod cls "calculateSizeOfFile:fork:estimating:" "Q@:@IB" stub_0

  -- prepareFileForBurn:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRFileDataProductionOverrides
    case _prepareFileForBurn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "prepareFileForBurn:" "B@:@" stub_1

  -- prepareFileForVerification:
  stub_2 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRFileDataProductionOverrides
    case _prepareFileForVerification rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "prepareFileForVerification:" "B@:@" stub_2

  -- cleanupFileAfterBurn:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRFileDataProductionOverrides
    case _cleanupFileAfterBurn rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cleanupFileAfterBurn:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO DRFileDataProductionOverrides
    if queriedSel == sel_calculateSizeOfFile_fork_estimating then pure (maybe 0 (const 1) (_calculateSizeOfFile_fork_estimating rec_))
    else if queriedSel == sel_prepareFileForBurn then pure (maybe 0 (const 1) (_prepareFileForBurn rec_))
    else if queriedSel == sel_prepareFileForVerification then pure (maybe 0 (const 1) (_prepareFileForVerification rec_))
    else if queriedSel == sel_cleanupFileAfterBurn then pure (maybe 0 (const 1) (_cleanupFileAfterBurn rec_))
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
newDRFileDataProduction :: DRFileDataProductionOverrides -> IO RawId
newDRFileDataProduction overrides = do
  inst <- class_createInstance drFileDataProductionDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
