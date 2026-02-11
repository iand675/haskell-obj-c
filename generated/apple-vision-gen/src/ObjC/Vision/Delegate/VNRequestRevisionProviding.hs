{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol VNRequestRevisionProviding@.
--
-- Usage:
--
-- @
-- delegate <- newVNRequestRevisionProviding defaultVNRequestRevisionProvidingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Vision.Delegate.VNRequestRevisionProviding
  ( VNRequestRevisionProvidingOverrides(..)
  , defaultVNRequestRevisionProvidingOverrides
  , newVNRequestRevisionProviding
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

-- | Overrides record for @\@protocol VNRequestRevisionProviding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data VNRequestRevisionProvidingOverrides = VNRequestRevisionProvidingOverrides
  { _requestRevision :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultVNRequestRevisionProvidingOverrides :: VNRequestRevisionProvidingOverrides
defaultVNRequestRevisionProvidingOverrides = VNRequestRevisionProvidingOverrides
  { _requestRevision = Nothing
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
{-# NOINLINE vnRequestRevisionProvidingDelegateClass #-}
vnRequestRevisionProvidingDelegateClass :: Class
vnRequestRevisionProvidingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsVNRequestRevisionProviding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_requestRevision = unSelector (mkSelector "requestRevision")
  -- requestRevision
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VNRequestRevisionProvidingOverrides
    case _requestRevision rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "requestRevision" "Q@:" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VNRequestRevisionProvidingOverrides
    if queriedSel == sel_requestRevision then pure (maybe 0 (const 1) (_requestRevision rec_))
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
newVNRequestRevisionProviding :: VNRequestRevisionProvidingOverrides -> IO RawId
newVNRequestRevisionProviding overrides = do
  inst <- class_createInstance vnRequestRevisionProvidingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
