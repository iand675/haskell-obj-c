{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol BEExtensionProcess@.
--
-- Usage:
--
-- @
-- delegate <- newBEExtensionProcess defaultBEExtensionProcessOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.BrowserEngineKit.Delegate.BEExtensionProcess
  ( BEExtensionProcessOverrides(..)
  , defaultBEExtensionProcessOverrides
  , newBEExtensionProcess
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

-- | Overrides record for @\@protocol BEExtensionProcess@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data BEExtensionProcessOverrides = BEExtensionProcessOverrides
  { _invalidate :: !(Maybe (IO ()))
  , _makeLibXPCConnectionError :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultBEExtensionProcessOverrides :: BEExtensionProcessOverrides
defaultBEExtensionProcessOverrides = BEExtensionProcessOverrides
  { _invalidate = Nothing
  , _makeLibXPCConnectionError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE beExtensionProcessDelegateClass #-}
beExtensionProcessDelegateClass :: Class
beExtensionProcessDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsBEExtensionProcess" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_invalidate = unSelector (mkSelector "invalidate")
      sel_makeLibXPCConnectionError = unSelector (mkSelector "makeLibXPCConnectionError:")
  -- invalidate
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEExtensionProcessOverrides
    case _invalidate rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "invalidate" "v@:" stub_0

  -- makeLibXPCConnectionError:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEExtensionProcessOverrides
    case _makeLibXPCConnectionError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "makeLibXPCConnectionError:" "@@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEExtensionProcessOverrides
    if queriedSel == sel_invalidate then pure (maybe 0 (const 1) (_invalidate rec_))
    else if queriedSel == sel_makeLibXPCConnectionError then pure (maybe 0 (const 1) (_makeLibXPCConnectionError rec_))
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
newBEExtensionProcess :: BEExtensionProcessOverrides -> IO RawId
newBEExtensionProcess overrides = do
  inst <- class_createInstance beExtensionProcessDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
