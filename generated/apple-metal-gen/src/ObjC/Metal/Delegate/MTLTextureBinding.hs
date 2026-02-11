{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLTextureBinding@.
--
-- Usage:
--
-- @
-- delegate <- newMTLTextureBinding defaultMTLTextureBindingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLTextureBinding
  ( MTLTextureBindingOverrides(..)
  , defaultMTLTextureBindingOverrides
  , newMTLTextureBinding
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

-- | Overrides record for @\@protocol MTLTextureBinding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLTextureBindingOverrides = MTLTextureBindingOverrides
  { _depthTexture :: !(Maybe (IO Bool))
  , _arrayLength :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLTextureBindingOverrides :: MTLTextureBindingOverrides
defaultMTLTextureBindingOverrides = MTLTextureBindingOverrides
  { _depthTexture = Nothing
  , _arrayLength = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlTextureBindingDelegateClass #-}
mtlTextureBindingDelegateClass :: Class
mtlTextureBindingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLTextureBinding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_depthTexture = unSelector (mkSelector "depthTexture")
      sel_arrayLength = unSelector (mkSelector "arrayLength")
  -- depthTexture
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLTextureBindingOverrides
    case _depthTexture rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "depthTexture" "B@:" stub_0

  -- arrayLength
  stub_1 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLTextureBindingOverrides
    case _arrayLength rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "arrayLength" "Q@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLTextureBindingOverrides
    if queriedSel == sel_depthTexture then pure (maybe 0 (const 1) (_depthTexture rec_))
    else if queriedSel == sel_arrayLength then pure (maybe 0 (const 1) (_arrayLength rec_))
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
newMTLTextureBinding :: MTLTextureBindingOverrides -> IO RawId
newMTLTextureBinding overrides = do
  inst <- class_createInstance mtlTextureBindingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
