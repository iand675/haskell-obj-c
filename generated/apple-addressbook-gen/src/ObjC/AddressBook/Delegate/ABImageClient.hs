{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ABImageClient@.
--
-- Usage:
--
-- @
-- delegate <- newABImageClient defaultABImageClientOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AddressBook.Delegate.ABImageClient
  ( ABImageClientOverrides(..)
  , defaultABImageClientOverrides
  , newABImageClient
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

-- | Overrides record for @\@protocol ABImageClient@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ABImageClientOverrides = ABImageClientOverrides
  { _consumeImageData_forTag :: !(Maybe (RawId -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultABImageClientOverrides :: ABImageClientOverrides
defaultABImageClientOverrides = ABImageClientOverrides
  { _consumeImageData_forTag = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE abImageClientDelegateClass #-}
abImageClientDelegateClass :: Class
abImageClientDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsABImageClient" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_consumeImageData_forTag = unSelector (mkSelector "consumeImageData:forTag:")
  -- consumeImageData:forTag:
  stub_0 <- wrap_at_q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ABImageClientOverrides
    case _consumeImageData_forTag rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "consumeImageData:forTag:" "v@:@q" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ABImageClientOverrides
    if queriedSel == sel_consumeImageData_forTag then pure (maybe 0 (const 1) (_consumeImageData_forTag rec_))
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
newABImageClient :: ABImageClientOverrides -> IO RawId
newABImageClient overrides = do
  inst <- class_createInstance abImageClientDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
