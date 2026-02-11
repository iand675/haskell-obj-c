{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLCommandBufferEncoderInfo@.
--
-- Usage:
--
-- @
-- delegate <- newMTLCommandBufferEncoderInfo defaultMTLCommandBufferEncoderInfoOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLCommandBufferEncoderInfo
  ( MTLCommandBufferEncoderInfoOverrides(..)
  , defaultMTLCommandBufferEncoderInfoOverrides
  , newMTLCommandBufferEncoderInfo
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

-- | Overrides record for @\@protocol MTLCommandBufferEncoderInfo@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLCommandBufferEncoderInfoOverrides = MTLCommandBufferEncoderInfoOverrides
  { _label :: !(Maybe (IO RawId))
  , _debugSignposts :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLCommandBufferEncoderInfoOverrides :: MTLCommandBufferEncoderInfoOverrides
defaultMTLCommandBufferEncoderInfoOverrides = MTLCommandBufferEncoderInfoOverrides
  { _label = Nothing
  , _debugSignposts = Nothing
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
{-# NOINLINE mtlCommandBufferEncoderInfoDelegateClass #-}
mtlCommandBufferEncoderInfoDelegateClass :: Class
mtlCommandBufferEncoderInfoDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLCommandBufferEncoderInfo" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_label = unSelector (mkSelector "label")
      sel_debugSignposts = unSelector (mkSelector "debugSignposts")
  -- label
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandBufferEncoderInfoOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_0

  -- debugSignposts
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandBufferEncoderInfoOverrides
    case _debugSignposts rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "debugSignposts" "@@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandBufferEncoderInfoOverrides
    if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_debugSignposts then pure (maybe 0 (const 1) (_debugSignposts rec_))
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
newMTLCommandBufferEncoderInfo :: MTLCommandBufferEncoderInfoOverrides -> IO RawId
newMTLCommandBufferEncoderInfo overrides = do
  inst <- class_createInstance mtlCommandBufferEncoderInfoDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
