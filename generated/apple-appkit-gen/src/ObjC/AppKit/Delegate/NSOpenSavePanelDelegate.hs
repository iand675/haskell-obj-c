{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSOpenSavePanelDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSOpenSavePanelDelegate defaultNSOpenSavePanelDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSOpenSavePanelDelegate
  ( NSOpenSavePanelDelegateOverrides(..)
  , defaultNSOpenSavePanelDelegateOverrides
  , newNSOpenSavePanelDelegate
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

-- | Overrides record for @\@protocol NSOpenSavePanelDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSOpenSavePanelDelegateOverrides = NSOpenSavePanelDelegateOverrides
  { _panel_shouldEnableURL :: !(Maybe (RawId -> RawId -> IO Bool))
  , _panel_validateURL_error :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _panel_didChangeToDirectoryURL :: !(Maybe (RawId -> RawId -> IO ()))
  , _panel_userEnteredFilename_confirmed :: !(Maybe (RawId -> RawId -> Bool -> IO RawId))
  , _panel_willExpand :: !(Maybe (RawId -> Bool -> IO ()))
  , _panelSelectionDidChange :: !(Maybe (RawId -> IO ()))
  , _panel_displayNameForType :: !(Maybe (RawId -> RawId -> IO RawId))
  , _panel_didSelectType :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSOpenSavePanelDelegateOverrides :: NSOpenSavePanelDelegateOverrides
defaultNSOpenSavePanelDelegateOverrides = NSOpenSavePanelDelegateOverrides
  { _panel_shouldEnableURL = Nothing
  , _panel_validateURL_error = Nothing
  , _panel_didChangeToDirectoryURL = Nothing
  , _panel_userEnteredFilename_confirmed = Nothing
  , _panel_willExpand = Nothing
  , _panelSelectionDidChange = Nothing
  , _panel_displayNameForType = Nothing
  , _panel_didSelectType = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsOpenSavePanelDelegateDelegateClass #-}
nsOpenSavePanelDelegateDelegateClass :: Class
nsOpenSavePanelDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSOpenSavePanelDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_panel_shouldEnableURL = unSelector (mkSelector "panel:shouldEnableURL:")
      sel_panel_validateURL_error = unSelector (mkSelector "panel:validateURL:error:")
      sel_panel_didChangeToDirectoryURL = unSelector (mkSelector "panel:didChangeToDirectoryURL:")
      sel_panel_userEnteredFilename_confirmed = unSelector (mkSelector "panel:userEnteredFilename:confirmed:")
      sel_panel_willExpand = unSelector (mkSelector "panel:willExpand:")
      sel_panelSelectionDidChange = unSelector (mkSelector "panelSelectionDidChange:")
      sel_panel_displayNameForType = unSelector (mkSelector "panel:displayNameForType:")
      sel_panel_didSelectType = unSelector (mkSelector "panel:didSelectType:")
  -- panel:shouldEnableURL:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOpenSavePanelDelegateOverrides
    case _panel_shouldEnableURL rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "panel:shouldEnableURL:" "B@:@@" stub_0

  -- panel:validateURL:error:
  stub_1 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOpenSavePanelDelegateOverrides
    case _panel_validateURL_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "panel:validateURL:error:" "B@:@@@" stub_1

  -- panel:didChangeToDirectoryURL:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOpenSavePanelDelegateOverrides
    case _panel_didChangeToDirectoryURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "panel:didChangeToDirectoryURL:" "v@:@@" stub_2

  -- panel:userEnteredFilename:confirmed:
  stub_3 <- wrap_at_at_B_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOpenSavePanelDelegateOverrides
    case _panel_userEnteredFilename_confirmed rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (arg2 /= 0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "panel:userEnteredFilename:confirmed:" "@@:@@B" stub_3

  -- panel:willExpand:
  stub_4 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOpenSavePanelDelegateOverrides
    case _panel_willExpand rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "panel:willExpand:" "v@:@B" stub_4

  -- panelSelectionDidChange:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOpenSavePanelDelegateOverrides
    case _panelSelectionDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "panelSelectionDidChange:" "v@:@" stub_5

  -- panel:displayNameForType:
  stub_6 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOpenSavePanelDelegateOverrides
    case _panel_displayNameForType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "panel:displayNameForType:" "@@:@@" stub_6

  -- panel:didSelectType:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOpenSavePanelDelegateOverrides
    case _panel_didSelectType rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "panel:didSelectType:" "v@:@@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOpenSavePanelDelegateOverrides
    if queriedSel == sel_panel_shouldEnableURL then pure (maybe 0 (const 1) (_panel_shouldEnableURL rec_))
    else if queriedSel == sel_panel_validateURL_error then pure (maybe 0 (const 1) (_panel_validateURL_error rec_))
    else if queriedSel == sel_panel_didChangeToDirectoryURL then pure (maybe 0 (const 1) (_panel_didChangeToDirectoryURL rec_))
    else if queriedSel == sel_panel_userEnteredFilename_confirmed then pure (maybe 0 (const 1) (_panel_userEnteredFilename_confirmed rec_))
    else if queriedSel == sel_panel_willExpand then pure (maybe 0 (const 1) (_panel_willExpand rec_))
    else if queriedSel == sel_panelSelectionDidChange then pure (maybe 0 (const 1) (_panelSelectionDidChange rec_))
    else if queriedSel == sel_panel_displayNameForType then pure (maybe 0 (const 1) (_panel_displayNameForType rec_))
    else if queriedSel == sel_panel_didSelectType then pure (maybe 0 (const 1) (_panel_didSelectType rec_))
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
newNSOpenSavePanelDelegate :: NSOpenSavePanelDelegateOverrides -> IO RawId
newNSOpenSavePanelDelegate overrides = do
  inst <- class_createInstance nsOpenSavePanelDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
