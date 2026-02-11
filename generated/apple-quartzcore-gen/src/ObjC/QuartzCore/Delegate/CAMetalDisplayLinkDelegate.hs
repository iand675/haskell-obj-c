{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CAMetalDisplayLinkDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCAMetalDisplayLinkDelegate defaultCAMetalDisplayLinkDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.QuartzCore.Delegate.CAMetalDisplayLinkDelegate
  ( CAMetalDisplayLinkDelegateOverrides(..)
  , defaultCAMetalDisplayLinkDelegateOverrides
  , newCAMetalDisplayLinkDelegate
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

-- | Overrides record for @\@protocol CAMetalDisplayLinkDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CAMetalDisplayLinkDelegateOverrides = CAMetalDisplayLinkDelegateOverrides
  { _metalDisplayLink_needsUpdate :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCAMetalDisplayLinkDelegateOverrides :: CAMetalDisplayLinkDelegateOverrides
defaultCAMetalDisplayLinkDelegateOverrides = CAMetalDisplayLinkDelegateOverrides
  { _metalDisplayLink_needsUpdate = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE caMetalDisplayLinkDelegateDelegateClass #-}
caMetalDisplayLinkDelegateDelegateClass :: Class
caMetalDisplayLinkDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCAMetalDisplayLinkDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_metalDisplayLink_needsUpdate = unSelector (mkSelector "metalDisplayLink:needsUpdate:")
  -- metalDisplayLink:needsUpdate:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMetalDisplayLinkDelegateOverrides
    case _metalDisplayLink_needsUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "metalDisplayLink:needsUpdate:" "v@:@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMetalDisplayLinkDelegateOverrides
    if queriedSel == sel_metalDisplayLink_needsUpdate then pure (maybe 0 (const 1) (_metalDisplayLink_needsUpdate rec_))
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
newCAMetalDisplayLinkDelegate :: CAMetalDisplayLinkDelegateOverrides -> IO RawId
newCAMetalDisplayLinkDelegate overrides = do
  inst <- class_createInstance caMetalDisplayLinkDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
