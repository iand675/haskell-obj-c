{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol WebOpenPanelResultListener@.
--
-- Usage:
--
-- @
-- delegate <- newWebOpenPanelResultListener defaultWebOpenPanelResultListenerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.WebKit.Delegate.WebOpenPanelResultListener
  ( WebOpenPanelResultListenerOverrides(..)
  , defaultWebOpenPanelResultListenerOverrides
  , newWebOpenPanelResultListener
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

-- | Overrides record for @\@protocol WebOpenPanelResultListener@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data WebOpenPanelResultListenerOverrides = WebOpenPanelResultListenerOverrides
  { _chooseFilename :: !(Maybe (RawId -> IO ()))
  , _chooseFilenames :: !(Maybe (RawId -> IO ()))
  , _cancel :: !(Maybe (IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultWebOpenPanelResultListenerOverrides :: WebOpenPanelResultListenerOverrides
defaultWebOpenPanelResultListenerOverrides = WebOpenPanelResultListenerOverrides
  { _chooseFilename = Nothing
  , _chooseFilenames = Nothing
  , _cancel = Nothing
  }

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE webOpenPanelResultListenerDelegateClass #-}
webOpenPanelResultListenerDelegateClass :: Class
webOpenPanelResultListenerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsWebOpenPanelResultListener" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_chooseFilename = unSelector (mkSelector "chooseFilename:")
      sel_chooseFilenames = unSelector (mkSelector "chooseFilenames:")
      sel_cancel = unSelector (mkSelector "cancel")
  -- chooseFilename:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebOpenPanelResultListenerOverrides
    case _chooseFilename rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "chooseFilename:" "v@:@" stub_0

  -- chooseFilenames:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebOpenPanelResultListenerOverrides
    case _chooseFilenames rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "chooseFilenames:" "v@:@" stub_1

  -- cancel
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebOpenPanelResultListenerOverrides
    case _cancel rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "cancel" "v@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebOpenPanelResultListenerOverrides
    if queriedSel == sel_chooseFilename then pure (maybe 0 (const 1) (_chooseFilename rec_))
    else if queriedSel == sel_chooseFilenames then pure (maybe 0 (const 1) (_chooseFilenames rec_))
    else if queriedSel == sel_cancel then pure (maybe 0 (const 1) (_cancel rec_))
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
newWebOpenPanelResultListener :: WebOpenPanelResultListenerOverrides -> IO RawId
newWebOpenPanelResultListener overrides = do
  inst <- class_createInstance webOpenPanelResultListenerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
