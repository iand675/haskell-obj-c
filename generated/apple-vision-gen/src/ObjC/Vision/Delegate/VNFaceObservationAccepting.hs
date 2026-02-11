{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol VNFaceObservationAccepting@.
--
-- Usage:
--
-- @
-- delegate <- newVNFaceObservationAccepting defaultVNFaceObservationAcceptingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Vision.Delegate.VNFaceObservationAccepting
  ( VNFaceObservationAcceptingOverrides(..)
  , defaultVNFaceObservationAcceptingOverrides
  , newVNFaceObservationAccepting
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

-- | Overrides record for @\@protocol VNFaceObservationAccepting@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data VNFaceObservationAcceptingOverrides = VNFaceObservationAcceptingOverrides
  { _inputFaceObservations :: !(Maybe (IO RawId))
  , _setInputFaceObservations :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultVNFaceObservationAcceptingOverrides :: VNFaceObservationAcceptingOverrides
defaultVNFaceObservationAcceptingOverrides = VNFaceObservationAcceptingOverrides
  { _inputFaceObservations = Nothing
  , _setInputFaceObservations = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE vnFaceObservationAcceptingDelegateClass #-}
vnFaceObservationAcceptingDelegateClass :: Class
vnFaceObservationAcceptingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsVNFaceObservationAccepting" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_inputFaceObservations = unSelector (mkSelector "inputFaceObservations")
      sel_setInputFaceObservations = unSelector (mkSelector "setInputFaceObservations:")
  -- inputFaceObservations
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VNFaceObservationAcceptingOverrides
    case _inputFaceObservations rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "inputFaceObservations" "@@:" stub_0

  -- setInputFaceObservations:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VNFaceObservationAcceptingOverrides
    case _setInputFaceObservations rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setInputFaceObservations:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VNFaceObservationAcceptingOverrides
    if queriedSel == sel_inputFaceObservations then pure (maybe 0 (const 1) (_inputFaceObservations rec_))
    else if queriedSel == sel_setInputFaceObservations then pure (maybe 0 (const 1) (_setInputFaceObservations rec_))
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
newVNFaceObservationAccepting :: VNFaceObservationAcceptingOverrides -> IO RawId
newVNFaceObservationAccepting overrides = do
  inst <- class_createInstance vnFaceObservationAcceptingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
