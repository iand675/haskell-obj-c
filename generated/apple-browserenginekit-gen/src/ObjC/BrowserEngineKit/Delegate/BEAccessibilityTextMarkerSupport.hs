{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol BEAccessibilityTextMarkerSupport@.
--
-- Usage:
--
-- @
-- delegate <- newBEAccessibilityTextMarkerSupport defaultBEAccessibilityTextMarkerSupportOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.BrowserEngineKit.Delegate.BEAccessibilityTextMarkerSupport
  ( BEAccessibilityTextMarkerSupportOverrides(..)
  , defaultBEAccessibilityTextMarkerSupportOverrides
  , newBEAccessibilityTextMarkerSupport
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

-- | Overrides record for @\@protocol BEAccessibilityTextMarkerSupport@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data BEAccessibilityTextMarkerSupportOverrides = BEAccessibilityTextMarkerSupportOverrides
  { _accessibilityContentForTextMarkerRange :: !(Maybe (RawId -> IO RawId))
  , _accessibilityTextMarkerRangeForCurrentSelection :: !(Maybe (IO RawId))
  , _accessibilityTextMarkerRange :: !(Maybe (IO RawId))
  , _accessibilityNextTextMarker :: !(Maybe (RawId -> IO RawId))
  , _accessibilityPreviousTextMarker :: !(Maybe (RawId -> IO RawId))
  , _accessibilityLineEndMarkerForMarker :: !(Maybe (RawId -> IO RawId))
  , _accessibilityLineStartMarkerForMarker :: !(Maybe (RawId -> IO RawId))
  , _accessibilityTextMarkerForPosition :: !(Maybe (Int -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultBEAccessibilityTextMarkerSupportOverrides :: BEAccessibilityTextMarkerSupportOverrides
defaultBEAccessibilityTextMarkerSupportOverrides = BEAccessibilityTextMarkerSupportOverrides
  { _accessibilityContentForTextMarkerRange = Nothing
  , _accessibilityTextMarkerRangeForCurrentSelection = Nothing
  , _accessibilityTextMarkerRange = Nothing
  , _accessibilityNextTextMarker = Nothing
  , _accessibilityPreviousTextMarker = Nothing
  , _accessibilityLineEndMarkerForMarker = Nothing
  , _accessibilityLineStartMarkerForMarker = Nothing
  , _accessibilityTextMarkerForPosition = Nothing
  }

foreign import ccall "wrapper"
  wrap_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE beAccessibilityTextMarkerSupportDelegateClass #-}
beAccessibilityTextMarkerSupportDelegateClass :: Class
beAccessibilityTextMarkerSupportDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsBEAccessibilityTextMarkerSupport" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityContentForTextMarkerRange = unSelector (mkSelector "accessibilityContentForTextMarkerRange:")
      sel_accessibilityTextMarkerRangeForCurrentSelection = unSelector (mkSelector "accessibilityTextMarkerRangeForCurrentSelection")
      sel_accessibilityTextMarkerRange = unSelector (mkSelector "accessibilityTextMarkerRange")
      sel_accessibilityNextTextMarker = unSelector (mkSelector "accessibilityNextTextMarker:")
      sel_accessibilityPreviousTextMarker = unSelector (mkSelector "accessibilityPreviousTextMarker:")
      sel_accessibilityLineEndMarkerForMarker = unSelector (mkSelector "accessibilityLineEndMarkerForMarker:")
      sel_accessibilityLineStartMarkerForMarker = unSelector (mkSelector "accessibilityLineStartMarkerForMarker:")
      sel_accessibilityTextMarkerForPosition = unSelector (mkSelector "accessibilityTextMarkerForPosition:")
  -- accessibilityContentForTextMarkerRange:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEAccessibilityTextMarkerSupportOverrides
    case _accessibilityContentForTextMarkerRange rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityContentForTextMarkerRange:" "@@:@" stub_0

  -- accessibilityTextMarkerRangeForCurrentSelection
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEAccessibilityTextMarkerSupportOverrides
    case _accessibilityTextMarkerRangeForCurrentSelection rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityTextMarkerRangeForCurrentSelection" "@@:" stub_1

  -- accessibilityTextMarkerRange
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEAccessibilityTextMarkerSupportOverrides
    case _accessibilityTextMarkerRange rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityTextMarkerRange" "@@:" stub_2

  -- accessibilityNextTextMarker:
  stub_3 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEAccessibilityTextMarkerSupportOverrides
    case _accessibilityNextTextMarker rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityNextTextMarker:" "@@:@" stub_3

  -- accessibilityPreviousTextMarker:
  stub_4 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEAccessibilityTextMarkerSupportOverrides
    case _accessibilityPreviousTextMarker rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityPreviousTextMarker:" "@@:@" stub_4

  -- accessibilityLineEndMarkerForMarker:
  stub_5 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEAccessibilityTextMarkerSupportOverrides
    case _accessibilityLineEndMarkerForMarker rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityLineEndMarkerForMarker:" "@@:@" stub_5

  -- accessibilityLineStartMarkerForMarker:
  stub_6 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEAccessibilityTextMarkerSupportOverrides
    case _accessibilityLineStartMarkerForMarker rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityLineStartMarkerForMarker:" "@@:@" stub_6

  -- accessibilityTextMarkerForPosition:
  stub_7 <- wrap_q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEAccessibilityTextMarkerSupportOverrides
    case _accessibilityTextMarkerForPosition rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityTextMarkerForPosition:" "@@:q" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BEAccessibilityTextMarkerSupportOverrides
    if queriedSel == sel_accessibilityContentForTextMarkerRange then pure (maybe 0 (const 1) (_accessibilityContentForTextMarkerRange rec_))
    else if queriedSel == sel_accessibilityTextMarkerRangeForCurrentSelection then pure (maybe 0 (const 1) (_accessibilityTextMarkerRangeForCurrentSelection rec_))
    else if queriedSel == sel_accessibilityTextMarkerRange then pure (maybe 0 (const 1) (_accessibilityTextMarkerRange rec_))
    else if queriedSel == sel_accessibilityNextTextMarker then pure (maybe 0 (const 1) (_accessibilityNextTextMarker rec_))
    else if queriedSel == sel_accessibilityPreviousTextMarker then pure (maybe 0 (const 1) (_accessibilityPreviousTextMarker rec_))
    else if queriedSel == sel_accessibilityLineEndMarkerForMarker then pure (maybe 0 (const 1) (_accessibilityLineEndMarkerForMarker rec_))
    else if queriedSel == sel_accessibilityLineStartMarkerForMarker then pure (maybe 0 (const 1) (_accessibilityLineStartMarkerForMarker rec_))
    else if queriedSel == sel_accessibilityTextMarkerForPosition then pure (maybe 0 (const 1) (_accessibilityTextMarkerForPosition rec_))
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
newBEAccessibilityTextMarkerSupport :: BEAccessibilityTextMarkerSupportOverrides -> IO RawId
newBEAccessibilityTextMarkerSupport overrides = do
  inst <- class_createInstance beAccessibilityTextMarkerSupportDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
