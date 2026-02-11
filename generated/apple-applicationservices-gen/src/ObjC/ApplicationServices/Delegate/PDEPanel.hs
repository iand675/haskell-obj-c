{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PDEPanel@.
--
-- Usage:
--
-- @
-- delegate <- newPDEPanel defaultPDEPanelOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ApplicationServices.Delegate.PDEPanel
  ( PDEPanelOverrides(..)
  , defaultPDEPanelOverrides
  , newPDEPanel
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

-- | Overrides record for @\@protocol PDEPanel@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PDEPanelOverrides = PDEPanelOverrides
  { _willShow :: !(Maybe (IO ()))
  , _shouldHide :: !(Maybe (IO Bool))
  , _saveValuesAndReturnError :: !(Maybe (RawId -> IO Bool))
  , _restoreValuesAndReturnError :: !(Maybe (RawId -> IO Bool))
  , _supportedPPDOptionKeys :: !(Maybe (IO RawId))
  , _ppdOptionKeyValueDidChange_ppdChoice :: !(Maybe (RawId -> RawId -> IO ()))
  , _panelView :: !(Maybe (IO RawId))
  , _panelName :: !(Maybe (IO RawId))
  , _panelKind :: !(Maybe (IO RawId))
  , _summaryInfo :: !(Maybe (IO RawId))
  , _shouldShowHelp :: !(Maybe (IO Bool))
  , _shouldPrint :: !(Maybe (IO Bool))
  , _printWindowWillClose :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPDEPanelOverrides :: PDEPanelOverrides
defaultPDEPanelOverrides = PDEPanelOverrides
  { _willShow = Nothing
  , _shouldHide = Nothing
  , _saveValuesAndReturnError = Nothing
  , _restoreValuesAndReturnError = Nothing
  , _supportedPPDOptionKeys = Nothing
  , _ppdOptionKeyValueDidChange_ppdChoice = Nothing
  , _panelView = Nothing
  , _panelName = Nothing
  , _panelKind = Nothing
  , _summaryInfo = Nothing
  , _shouldShowHelp = Nothing
  , _shouldPrint = Nothing
  , _printWindowWillClose = Nothing
  }

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pdePanelDelegateClass #-}
pdePanelDelegateClass :: Class
pdePanelDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPDEPanel" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_willShow = unSelector (mkSelector "willShow")
      sel_shouldHide = unSelector (mkSelector "shouldHide")
      sel_saveValuesAndReturnError = unSelector (mkSelector "saveValuesAndReturnError:")
      sel_restoreValuesAndReturnError = unSelector (mkSelector "restoreValuesAndReturnError:")
      sel_supportedPPDOptionKeys = unSelector (mkSelector "supportedPPDOptionKeys")
      sel_ppdOptionKeyValueDidChange_ppdChoice = unSelector (mkSelector "PPDOptionKeyValueDidChange:ppdChoice:")
      sel_panelView = unSelector (mkSelector "panelView")
      sel_panelName = unSelector (mkSelector "panelName")
      sel_panelKind = unSelector (mkSelector "panelKind")
      sel_summaryInfo = unSelector (mkSelector "summaryInfo")
      sel_shouldShowHelp = unSelector (mkSelector "shouldShowHelp")
      sel_shouldPrint = unSelector (mkSelector "shouldPrint")
      sel_printWindowWillClose = unSelector (mkSelector "printWindowWillClose:")
  -- willShow
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _willShow rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "willShow" "v@:" stub_0

  -- shouldHide
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _shouldHide rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldHide" "B@:" stub_1

  -- saveValuesAndReturnError:
  stub_2 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _saveValuesAndReturnError rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "saveValuesAndReturnError:" "B@:@" stub_2

  -- restoreValuesAndReturnError:
  stub_3 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _restoreValuesAndReturnError rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "restoreValuesAndReturnError:" "B@:@" stub_3

  -- supportedPPDOptionKeys
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _supportedPPDOptionKeys rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedPPDOptionKeys" "@@:" stub_4

  -- PPDOptionKeyValueDidChange:ppdChoice:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _ppdOptionKeyValueDidChange_ppdChoice rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "PPDOptionKeyValueDidChange:ppdChoice:" "v@:@@" stub_5

  -- panelView
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _panelView rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "panelView" "@@:" stub_6

  -- panelName
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _panelName rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "panelName" "@@:" stub_7

  -- panelKind
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _panelKind rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "panelKind" "@@:" stub_8

  -- summaryInfo
  stub_9 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _summaryInfo rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "summaryInfo" "@@:" stub_9

  -- shouldShowHelp
  stub_10 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _shouldShowHelp rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldShowHelp" "B@:" stub_10

  -- shouldPrint
  stub_11 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _shouldPrint rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldPrint" "B@:" stub_11

  -- printWindowWillClose:
  stub_12 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    case _printWindowWillClose rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "printWindowWillClose:" "v@:B" stub_12

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPanelOverrides
    if queriedSel == sel_willShow then pure (maybe 0 (const 1) (_willShow rec_))
    else if queriedSel == sel_shouldHide then pure (maybe 0 (const 1) (_shouldHide rec_))
    else if queriedSel == sel_saveValuesAndReturnError then pure (maybe 0 (const 1) (_saveValuesAndReturnError rec_))
    else if queriedSel == sel_restoreValuesAndReturnError then pure (maybe 0 (const 1) (_restoreValuesAndReturnError rec_))
    else if queriedSel == sel_supportedPPDOptionKeys then pure (maybe 0 (const 1) (_supportedPPDOptionKeys rec_))
    else if queriedSel == sel_ppdOptionKeyValueDidChange_ppdChoice then pure (maybe 0 (const 1) (_ppdOptionKeyValueDidChange_ppdChoice rec_))
    else if queriedSel == sel_panelView then pure (maybe 0 (const 1) (_panelView rec_))
    else if queriedSel == sel_panelName then pure (maybe 0 (const 1) (_panelName rec_))
    else if queriedSel == sel_panelKind then pure (maybe 0 (const 1) (_panelKind rec_))
    else if queriedSel == sel_summaryInfo then pure (maybe 0 (const 1) (_summaryInfo rec_))
    else if queriedSel == sel_shouldShowHelp then pure (maybe 0 (const 1) (_shouldShowHelp rec_))
    else if queriedSel == sel_shouldPrint then pure (maybe 0 (const 1) (_shouldPrint rec_))
    else if queriedSel == sel_printWindowWillClose then pure (maybe 0 (const 1) (_printWindowWillClose rec_))
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
newPDEPanel :: PDEPanelOverrides -> IO RawId
newPDEPanel overrides = do
  inst <- class_createInstance pdePanelDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
