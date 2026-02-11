{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTRKeypair@.
--
-- Usage:
--
-- @
-- delegate <- newMTRKeypair defaultMTRKeypairOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTRKeypair
  ( MTRKeypairOverrides(..)
  , defaultMTRKeypairOverrides
  , newMTRKeypair
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

-- | Overrides record for @\@protocol MTRKeypair@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTRKeypairOverrides = MTRKeypairOverrides
  { _signMessageECDSA_RAW :: !(Maybe (RawId -> IO RawId))
  , _signMessageECDSA_DER :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTRKeypairOverrides :: MTRKeypairOverrides
defaultMTRKeypairOverrides = MTRKeypairOverrides
  { _signMessageECDSA_RAW = Nothing
  , _signMessageECDSA_DER = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtrKeypairDelegateClass #-}
mtrKeypairDelegateClass :: Class
mtrKeypairDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTRKeypair" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_signMessageECDSA_RAW = unSelector (mkSelector "signMessageECDSA_RAW:")
      sel_signMessageECDSA_DER = unSelector (mkSelector "signMessageECDSA_DER:")
  -- signMessageECDSA_RAW:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRKeypairOverrides
    case _signMessageECDSA_RAW rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "signMessageECDSA_RAW:" "@@:@" stub_0

  -- signMessageECDSA_DER:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRKeypairOverrides
    case _signMessageECDSA_DER rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "signMessageECDSA_DER:" "@@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRKeypairOverrides
    if queriedSel == sel_signMessageECDSA_RAW then pure (maybe 0 (const 1) (_signMessageECDSA_RAW rec_))
    else if queriedSel == sel_signMessageECDSA_DER then pure (maybe 0 (const 1) (_signMessageECDSA_DER rec_))
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
newMTRKeypair :: MTRKeypairOverrides -> IO RawId
newMTRKeypair overrides = do
  inst <- class_createInstance mtrKeypairDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
