{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MPSImageSizeEncodingState@.
--
-- Usage:
--
-- @
-- delegate <- newMPSImageSizeEncodingState defaultMPSImageSizeEncodingStateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MetalPerformanceShaders.Delegate.MPSImageSizeEncodingState
  ( MPSImageSizeEncodingStateOverrides(..)
  , defaultMPSImageSizeEncodingStateOverrides
  , newMPSImageSizeEncodingState
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

-- | Overrides record for @\@protocol MPSImageSizeEncodingState@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MPSImageSizeEncodingStateOverrides = MPSImageSizeEncodingStateOverrides
  { _sourceWidth :: !(Maybe (IO Int))
  , _sourceHeight :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMPSImageSizeEncodingStateOverrides :: MPSImageSizeEncodingStateOverrides
defaultMPSImageSizeEncodingStateOverrides = MPSImageSizeEncodingStateOverrides
  { _sourceWidth = Nothing
  , _sourceHeight = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mpsImageSizeEncodingStateDelegateClass #-}
mpsImageSizeEncodingStateDelegateClass :: Class
mpsImageSizeEncodingStateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMPSImageSizeEncodingState" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sourceWidth = unSelector (mkSelector "sourceWidth")
      sel_sourceHeight = unSelector (mkSelector "sourceHeight")
  -- sourceWidth
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSImageSizeEncodingStateOverrides
    case _sourceWidth rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "sourceWidth" "Q@:" stub_0

  -- sourceHeight
  stub_1 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSImageSizeEncodingStateOverrides
    case _sourceHeight rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "sourceHeight" "Q@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSImageSizeEncodingStateOverrides
    if queriedSel == sel_sourceWidth then pure (maybe 0 (const 1) (_sourceWidth rec_))
    else if queriedSel == sel_sourceHeight then pure (maybe 0 (const 1) (_sourceHeight rec_))
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
newMPSImageSizeEncodingState :: MPSImageSizeEncodingStateOverrides -> IO RawId
newMPSImageSizeEncodingState overrides = do
  inst <- class_createInstance mpsImageSizeEncodingStateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
