{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextLayoutManagerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextLayoutManagerDelegate defaultNSTextLayoutManagerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextLayoutManagerDelegate
  ( NSTextLayoutManagerDelegateOverrides(..)
  , defaultNSTextLayoutManagerDelegateOverrides
  , newNSTextLayoutManagerDelegate
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

-- | Overrides record for @\@protocol NSTextLayoutManagerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextLayoutManagerDelegateOverrides = NSTextLayoutManagerDelegateOverrides
  { _textLayoutManager_textLayoutFragmentForLocation_inTextElement :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _textLayoutManager_shouldBreakLineBeforeLocation_hyphenating :: !(Maybe (RawId -> RawId -> Bool -> IO Bool))
  , _textLayoutManager_renderingAttributesForLink_atLocation_defaultAttributes :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextLayoutManagerDelegateOverrides :: NSTextLayoutManagerDelegateOverrides
defaultNSTextLayoutManagerDelegateOverrides = NSTextLayoutManagerDelegateOverrides
  { _textLayoutManager_textLayoutFragmentForLocation_inTextElement = Nothing
  , _textLayoutManager_shouldBreakLineBeforeLocation_hyphenating = Nothing
  , _textLayoutManager_renderingAttributesForLink_atLocation_defaultAttributes = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_B_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTextLayoutManagerDelegateDelegateClass #-}
nsTextLayoutManagerDelegateDelegateClass :: Class
nsTextLayoutManagerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextLayoutManagerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_textLayoutManager_textLayoutFragmentForLocation_inTextElement = unSelector (mkSelector "textLayoutManager:textLayoutFragmentForLocation:inTextElement:")
      sel_textLayoutManager_shouldBreakLineBeforeLocation_hyphenating = unSelector (mkSelector "textLayoutManager:shouldBreakLineBeforeLocation:hyphenating:")
      sel_textLayoutManager_renderingAttributesForLink_atLocation_defaultAttributes = unSelector (mkSelector "textLayoutManager:renderingAttributesForLink:atLocation:defaultAttributes:")
  -- textLayoutManager:textLayoutFragmentForLocation:inTextElement:
  stub_0 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextLayoutManagerDelegateOverrides
    case _textLayoutManager_textLayoutFragmentForLocation_inTextElement rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textLayoutManager:textLayoutFragmentForLocation:inTextElement:" "@@:@@@" stub_0

  -- textLayoutManager:shouldBreakLineBeforeLocation:hyphenating:
  stub_1 <- wrap_at_at_B_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextLayoutManagerDelegateOverrides
    case _textLayoutManager_shouldBreakLineBeforeLocation_hyphenating rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (arg2 /= 0)
        pure (if r then 1 else 0)
  addObjCMethod cls "textLayoutManager:shouldBreakLineBeforeLocation:hyphenating:" "B@:@@B" stub_1

  -- textLayoutManager:renderingAttributesForLink:atLocation:defaultAttributes:
  stub_2 <- wrap_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextLayoutManagerDelegateOverrides
    case _textLayoutManager_renderingAttributesForLink_atLocation_defaultAttributes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textLayoutManager:renderingAttributesForLink:atLocation:defaultAttributes:" "@@:@@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextLayoutManagerDelegateOverrides
    if queriedSel == sel_textLayoutManager_textLayoutFragmentForLocation_inTextElement then pure (maybe 0 (const 1) (_textLayoutManager_textLayoutFragmentForLocation_inTextElement rec_))
    else if queriedSel == sel_textLayoutManager_shouldBreakLineBeforeLocation_hyphenating then pure (maybe 0 (const 1) (_textLayoutManager_shouldBreakLineBeforeLocation_hyphenating rec_))
    else if queriedSel == sel_textLayoutManager_renderingAttributesForLink_atLocation_defaultAttributes then pure (maybe 0 (const 1) (_textLayoutManager_renderingAttributesForLink_atLocation_defaultAttributes rec_))
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
newNSTextLayoutManagerDelegate :: NSTextLayoutManagerDelegateOverrides -> IO RawId
newNSTextLayoutManagerDelegate overrides = do
  inst <- class_createInstance nsTextLayoutManagerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
