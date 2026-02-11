{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol BEProcessCapabilityGrant@.
--
-- Usage:
--
-- @
-- delegate <- newBEProcessCapabilityGrant defaultBEProcessCapabilityGrantOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.BrowserEngineKit.Delegate.BEProcessCapabilityGrant
  ( BEProcessCapabilityGrantOverrides(..)
  , defaultBEProcessCapabilityGrantOverrides
  , newBEProcessCapabilityGrant
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

-- | Overrides record for @\@protocol BEProcessCapabilityGrant@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data BEProcessCapabilityGrantOverrides = BEProcessCapabilityGrantOverrides
  { _invalidate :: !(Maybe (IO Bool))
  , _valid :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultBEProcessCapabilityGrantOverrides :: BEProcessCapabilityGrantOverrides
defaultBEProcessCapabilityGrantOverrides = BEProcessCapabilityGrantOverrides
  { _invalidate = Nothing
  , _valid = Nothing
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
{-# NOINLINE beProcessCapabilityGrantDelegateClass #-}
beProcessCapabilityGrantDelegateClass :: Class
beProcessCapabilityGrantDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsBEProcessCapabilityGrant" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_invalidate = unSelector (mkSelector "invalidate")
      sel_valid = unSelector (mkSelector "valid")
  -- invalidate
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEProcessCapabilityGrantOverrides
    case _invalidate rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "invalidate" "B@:" stub_0

  -- valid
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEProcessCapabilityGrantOverrides
    case _valid rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "valid" "B@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEProcessCapabilityGrantOverrides
    if queriedSel == sel_invalidate then pure (maybe 0 (const 1) (_invalidate rec_))
    else if queriedSel == sel_valid then pure (maybe 0 (const 1) (_valid rec_))
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
newBEProcessCapabilityGrant :: BEProcessCapabilityGrantOverrides -> IO RawId
newBEProcessCapabilityGrant overrides = do
  inst <- class_createInstance beProcessCapabilityGrantDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
