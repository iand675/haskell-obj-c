{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NCWidgetSearchViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNCWidgetSearchViewDelegate defaultNCWidgetSearchViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.NotificationCenter.Delegate.NCWidgetSearchViewDelegate
  ( NCWidgetSearchViewDelegateOverrides(..)
  , defaultNCWidgetSearchViewDelegateOverrides
  , newNCWidgetSearchViewDelegate
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

-- | Overrides record for @\@protocol NCWidgetSearchViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NCWidgetSearchViewDelegateOverrides = NCWidgetSearchViewDelegateOverrides
  { _widgetSearch_searchForTerm_maxResults :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  , _widgetSearchTermCleared :: !(Maybe (RawId -> IO ()))
  , _widgetSearch_resultSelected :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNCWidgetSearchViewDelegateOverrides :: NCWidgetSearchViewDelegateOverrides
defaultNCWidgetSearchViewDelegateOverrides = NCWidgetSearchViewDelegateOverrides
  { _widgetSearch_searchForTerm_maxResults = Nothing
  , _widgetSearchTermCleared = Nothing
  , _widgetSearch_resultSelected = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ncWidgetSearchViewDelegateDelegateClass #-}
ncWidgetSearchViewDelegateDelegateClass :: Class
ncWidgetSearchViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNCWidgetSearchViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_widgetSearch_searchForTerm_maxResults = unSelector (mkSelector "widgetSearch:searchForTerm:maxResults:")
      sel_widgetSearchTermCleared = unSelector (mkSelector "widgetSearchTermCleared:")
      sel_widgetSearch_resultSelected = unSelector (mkSelector "widgetSearch:resultSelected:")
  -- widgetSearch:searchForTerm:maxResults:
  stub_0 <- wrap_at_at_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetSearchViewDelegateOverrides
    case _widgetSearch_searchForTerm_maxResults rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "widgetSearch:searchForTerm:maxResults:" "v@:@@Q" stub_0

  -- widgetSearchTermCleared:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetSearchViewDelegateOverrides
    case _widgetSearchTermCleared rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "widgetSearchTermCleared:" "v@:@" stub_1

  -- widgetSearch:resultSelected:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetSearchViewDelegateOverrides
    case _widgetSearch_resultSelected rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "widgetSearch:resultSelected:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetSearchViewDelegateOverrides
    if queriedSel == sel_widgetSearch_searchForTerm_maxResults then pure (maybe 0 (const 1) (_widgetSearch_searchForTerm_maxResults rec_))
    else if queriedSel == sel_widgetSearchTermCleared then pure (maybe 0 (const 1) (_widgetSearchTermCleared rec_))
    else if queriedSel == sel_widgetSearch_resultSelected then pure (maybe 0 (const 1) (_widgetSearch_resultSelected rec_))
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
newNCWidgetSearchViewDelegate :: NCWidgetSearchViewDelegateOverrides -> IO RawId
newNCWidgetSearchViewDelegate overrides = do
  inst <- class_createInstance ncWidgetSearchViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
