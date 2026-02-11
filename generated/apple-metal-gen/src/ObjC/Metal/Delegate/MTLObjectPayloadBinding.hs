{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLObjectPayloadBinding@.
--
-- Usage:
--
-- @
-- delegate <- newMTLObjectPayloadBinding defaultMTLObjectPayloadBindingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLObjectPayloadBinding
  ( MTLObjectPayloadBindingOverrides(..)
  , defaultMTLObjectPayloadBindingOverrides
  , newMTLObjectPayloadBinding
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

-- | Overrides record for @\@protocol MTLObjectPayloadBinding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLObjectPayloadBindingOverrides = MTLObjectPayloadBindingOverrides
  { _objectPayloadAlignment :: !(Maybe (IO Int))
  , _objectPayloadDataSize :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLObjectPayloadBindingOverrides :: MTLObjectPayloadBindingOverrides
defaultMTLObjectPayloadBindingOverrides = MTLObjectPayloadBindingOverrides
  { _objectPayloadAlignment = Nothing
  , _objectPayloadDataSize = Nothing
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
{-# NOINLINE mtlObjectPayloadBindingDelegateClass #-}
mtlObjectPayloadBindingDelegateClass :: Class
mtlObjectPayloadBindingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLObjectPayloadBinding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_objectPayloadAlignment = unSelector (mkSelector "objectPayloadAlignment")
      sel_objectPayloadDataSize = unSelector (mkSelector "objectPayloadDataSize")
  -- objectPayloadAlignment
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLObjectPayloadBindingOverrides
    case _objectPayloadAlignment rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "objectPayloadAlignment" "Q@:" stub_0

  -- objectPayloadDataSize
  stub_1 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLObjectPayloadBindingOverrides
    case _objectPayloadDataSize rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "objectPayloadDataSize" "Q@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLObjectPayloadBindingOverrides
    if queriedSel == sel_objectPayloadAlignment then pure (maybe 0 (const 1) (_objectPayloadAlignment rec_))
    else if queriedSel == sel_objectPayloadDataSize then pure (maybe 0 (const 1) (_objectPayloadDataSize rec_))
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
newMTLObjectPayloadBinding :: MTLObjectPayloadBindingOverrides -> IO RawId
newMTLObjectPayloadBinding overrides = do
  inst <- class_createInstance mtlObjectPayloadBindingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
