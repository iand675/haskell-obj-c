{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderEnumerationObserver@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderEnumerationObserver defaultNSFileProviderEnumerationObserverOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderEnumerationObserver
  ( NSFileProviderEnumerationObserverOverrides(..)
  , defaultNSFileProviderEnumerationObserverOverrides
  , newNSFileProviderEnumerationObserver
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

-- | Overrides record for @\@protocol NSFileProviderEnumerationObserver@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderEnumerationObserverOverrides = NSFileProviderEnumerationObserverOverrides
  { _didEnumerateItems :: !(Maybe (RawId -> IO ()))
  , _finishEnumeratingUpToPage :: !(Maybe (RawId -> IO ()))
  , _finishEnumeratingWithError :: !(Maybe (RawId -> IO ()))
  , _suggestedPageSize :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderEnumerationObserverOverrides :: NSFileProviderEnumerationObserverOverrides
defaultNSFileProviderEnumerationObserverOverrides = NSFileProviderEnumerationObserverOverrides
  { _didEnumerateItems = Nothing
  , _finishEnumeratingUpToPage = Nothing
  , _finishEnumeratingWithError = Nothing
  , _suggestedPageSize = Nothing
  }

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsFileProviderEnumerationObserverDelegateClass #-}
nsFileProviderEnumerationObserverDelegateClass :: Class
nsFileProviderEnumerationObserverDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderEnumerationObserver" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_didEnumerateItems = unSelector (mkSelector "didEnumerateItems:")
      sel_finishEnumeratingUpToPage = unSelector (mkSelector "finishEnumeratingUpToPage:")
      sel_finishEnumeratingWithError = unSelector (mkSelector "finishEnumeratingWithError:")
      sel_suggestedPageSize = unSelector (mkSelector "suggestedPageSize")
  -- didEnumerateItems:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderEnumerationObserverOverrides
    case _didEnumerateItems rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didEnumerateItems:" "v@:@" stub_0

  -- finishEnumeratingUpToPage:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderEnumerationObserverOverrides
    case _finishEnumeratingUpToPage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "finishEnumeratingUpToPage:" "v@:@" stub_1

  -- finishEnumeratingWithError:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderEnumerationObserverOverrides
    case _finishEnumeratingWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "finishEnumeratingWithError:" "v@:@" stub_2

  -- suggestedPageSize
  stub_3 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderEnumerationObserverOverrides
    case _suggestedPageSize rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "suggestedPageSize" "q@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderEnumerationObserverOverrides
    if queriedSel == sel_didEnumerateItems then pure (maybe 0 (const 1) (_didEnumerateItems rec_))
    else if queriedSel == sel_finishEnumeratingUpToPage then pure (maybe 0 (const 1) (_finishEnumeratingUpToPage rec_))
    else if queriedSel == sel_finishEnumeratingWithError then pure (maybe 0 (const 1) (_finishEnumeratingWithError rec_))
    else if queriedSel == sel_suggestedPageSize then pure (maybe 0 (const 1) (_suggestedPageSize rec_))
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
newNSFileProviderEnumerationObserver :: NSFileProviderEnumerationObserverOverrides -> IO RawId
newNSFileProviderEnumerationObserver overrides = do
  inst <- class_createInstance nsFileProviderEnumerationObserverDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
