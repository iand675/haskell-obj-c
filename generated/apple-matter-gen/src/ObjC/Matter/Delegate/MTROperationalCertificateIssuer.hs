{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTROperationalCertificateIssuer@.
--
-- Usage:
--
-- @
-- delegate <- newMTROperationalCertificateIssuer defaultMTROperationalCertificateIssuerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTROperationalCertificateIssuer
  ( MTROperationalCertificateIssuerOverrides(..)
  , defaultMTROperationalCertificateIssuerOverrides
  , newMTROperationalCertificateIssuer
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

-- | Overrides record for @\@protocol MTROperationalCertificateIssuer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTROperationalCertificateIssuerOverrides = MTROperationalCertificateIssuerOverrides
  { _shouldSkipAttestationCertificateValidation :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultMTROperationalCertificateIssuerOverrides :: MTROperationalCertificateIssuerOverrides
defaultMTROperationalCertificateIssuerOverrides = MTROperationalCertificateIssuerOverrides
  { _shouldSkipAttestationCertificateValidation = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtrOperationalCertificateIssuerDelegateClass #-}
mtrOperationalCertificateIssuerDelegateClass :: Class
mtrOperationalCertificateIssuerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTROperationalCertificateIssuer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_shouldSkipAttestationCertificateValidation = unSelector (mkSelector "shouldSkipAttestationCertificateValidation")
  -- shouldSkipAttestationCertificateValidation
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTROperationalCertificateIssuerOverrides
    case _shouldSkipAttestationCertificateValidation rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldSkipAttestationCertificateValidation" "B@:" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTROperationalCertificateIssuerOverrides
    if queriedSel == sel_shouldSkipAttestationCertificateValidation then pure (maybe 0 (const 1) (_shouldSkipAttestationCertificateValidation rec_))
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
newMTROperationalCertificateIssuer :: MTROperationalCertificateIssuerOverrides -> IO RawId
newMTROperationalCertificateIssuer overrides = do
  inst <- class_createInstance mtrOperationalCertificateIssuerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
