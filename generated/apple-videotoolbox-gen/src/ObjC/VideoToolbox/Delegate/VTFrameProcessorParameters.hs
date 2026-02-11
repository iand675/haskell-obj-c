{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol VTFrameProcessorParameters@.
--
-- Usage:
--
-- @
-- delegate <- newVTFrameProcessorParameters defaultVTFrameProcessorParametersOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.VideoToolbox.Delegate.VTFrameProcessorParameters
  ( VTFrameProcessorParametersOverrides(..)
  , defaultVTFrameProcessorParametersOverrides
  , newVTFrameProcessorParameters
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

-- | Overrides record for @\@protocol VTFrameProcessorParameters@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data VTFrameProcessorParametersOverrides = VTFrameProcessorParametersOverrides
  { _sourceFrame :: !(Maybe (IO RawId))
  , _destinationFrame :: !(Maybe (IO RawId))
  , _destinationFrames :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultVTFrameProcessorParametersOverrides :: VTFrameProcessorParametersOverrides
defaultVTFrameProcessorParametersOverrides = VTFrameProcessorParametersOverrides
  { _sourceFrame = Nothing
  , _destinationFrame = Nothing
  , _destinationFrames = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE vtFrameProcessorParametersDelegateClass #-}
vtFrameProcessorParametersDelegateClass :: Class
vtFrameProcessorParametersDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsVTFrameProcessorParameters" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sourceFrame = unSelector (mkSelector "sourceFrame")
      sel_destinationFrame = unSelector (mkSelector "destinationFrame")
      sel_destinationFrames = unSelector (mkSelector "destinationFrames")
  -- sourceFrame
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorParametersOverrides
    case _sourceFrame rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sourceFrame" "@@:" stub_0

  -- destinationFrame
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorParametersOverrides
    case _destinationFrame rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "destinationFrame" "@@:" stub_1

  -- destinationFrames
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorParametersOverrides
    case _destinationFrames rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "destinationFrames" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorParametersOverrides
    if queriedSel == sel_sourceFrame then pure (maybe 0 (const 1) (_sourceFrame rec_))
    else if queriedSel == sel_destinationFrame then pure (maybe 0 (const 1) (_destinationFrame rec_))
    else if queriedSel == sel_destinationFrames then pure (maybe 0 (const 1) (_destinationFrames rec_))
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
newVTFrameProcessorParameters :: VTFrameProcessorParametersOverrides -> IO RawId
newVTFrameProcessorParameters overrides = do
  inst <- class_createInstance vtFrameProcessorParametersDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
