{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSLayoutManagerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSLayoutManagerDelegate defaultNSLayoutManagerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSLayoutManagerDelegate
  ( NSLayoutManagerDelegateOverrides(..)
  , defaultNSLayoutManagerDelegateOverrides
  , newNSLayoutManagerDelegate
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

-- | Overrides record for @\@protocol NSLayoutManagerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSLayoutManagerDelegateOverrides = NSLayoutManagerDelegateOverrides
  { _layoutManager_shouldBreakLineByWordBeforeCharacterAtIndex :: !(Maybe (RawId -> Int -> IO Bool))
  , _layoutManager_shouldBreakLineByHyphenatingBeforeCharacterAtIndex :: !(Maybe (RawId -> Int -> IO Bool))
  , _layoutManagerDidInvalidateLayout :: !(Maybe (RawId -> IO ()))
  , _layoutManager_didCompleteLayoutForTextContainer_atEnd :: !(Maybe (RawId -> RawId -> Bool -> IO ()))
  , _layoutManager_shouldUseTemporaryAttributes_forDrawingToScreen_atCharacterIndex_effectiveRange :: !(Maybe (RawId -> RawId -> Bool -> Int -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSLayoutManagerDelegateOverrides :: NSLayoutManagerDelegateOverrides
defaultNSLayoutManagerDelegateOverrides = NSLayoutManagerDelegateOverrides
  { _layoutManager_shouldBreakLineByWordBeforeCharacterAtIndex = Nothing
  , _layoutManager_shouldBreakLineByHyphenatingBeforeCharacterAtIndex = Nothing
  , _layoutManagerDidInvalidateLayout = Nothing
  , _layoutManager_didCompleteLayoutForTextContainer_atEnd = Nothing
  , _layoutManager_shouldUseTemporaryAttributes_forDrawingToScreen_atCharacterIndex_effectiveRange = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B_Q_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsLayoutManagerDelegateDelegateClass #-}
nsLayoutManagerDelegateDelegateClass :: Class
nsLayoutManagerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSLayoutManagerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_layoutManager_shouldBreakLineByWordBeforeCharacterAtIndex = unSelector (mkSelector "layoutManager:shouldBreakLineByWordBeforeCharacterAtIndex:")
      sel_layoutManager_shouldBreakLineByHyphenatingBeforeCharacterAtIndex = unSelector (mkSelector "layoutManager:shouldBreakLineByHyphenatingBeforeCharacterAtIndex:")
      sel_layoutManagerDidInvalidateLayout = unSelector (mkSelector "layoutManagerDidInvalidateLayout:")
      sel_layoutManager_didCompleteLayoutForTextContainer_atEnd = unSelector (mkSelector "layoutManager:didCompleteLayoutForTextContainer:atEnd:")
      sel_layoutManager_shouldUseTemporaryAttributes_forDrawingToScreen_atCharacterIndex_effectiveRange = unSelector (mkSelector "layoutManager:shouldUseTemporaryAttributes:forDrawingToScreen:atCharacterIndex:effectiveRange:")
  -- layoutManager:shouldBreakLineByWordBeforeCharacterAtIndex:
  stub_0 <- wrap_at_Q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSLayoutManagerDelegateOverrides
    case _layoutManager_shouldBreakLineByWordBeforeCharacterAtIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "layoutManager:shouldBreakLineByWordBeforeCharacterAtIndex:" "B@:@Q" stub_0

  -- layoutManager:shouldBreakLineByHyphenatingBeforeCharacterAtIndex:
  stub_1 <- wrap_at_Q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSLayoutManagerDelegateOverrides
    case _layoutManager_shouldBreakLineByHyphenatingBeforeCharacterAtIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "layoutManager:shouldBreakLineByHyphenatingBeforeCharacterAtIndex:" "B@:@Q" stub_1

  -- layoutManagerDidInvalidateLayout:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSLayoutManagerDelegateOverrides
    case _layoutManagerDidInvalidateLayout rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "layoutManagerDidInvalidateLayout:" "v@:@" stub_2

  -- layoutManager:didCompleteLayoutForTextContainer:atEnd:
  stub_3 <- wrap_at_at_B_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSLayoutManagerDelegateOverrides
    case _layoutManager_didCompleteLayoutForTextContainer_atEnd rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (arg2 /= 0)
  addObjCMethod cls "layoutManager:didCompleteLayoutForTextContainer:atEnd:" "v@:@@B" stub_3

  -- layoutManager:shouldUseTemporaryAttributes:forDrawingToScreen:atCharacterIndex:effectiveRange:
  stub_4 <- wrap_at_at_B_Q_at_at $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSLayoutManagerDelegateOverrides
    case _layoutManager_shouldUseTemporaryAttributes_forDrawingToScreen_atCharacterIndex_effectiveRange rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (arg2 /= 0) (fromIntegral arg3) (RawId arg4)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "layoutManager:shouldUseTemporaryAttributes:forDrawingToScreen:atCharacterIndex:effectiveRange:" "@@:@@BQ@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSLayoutManagerDelegateOverrides
    if queriedSel == sel_layoutManager_shouldBreakLineByWordBeforeCharacterAtIndex then pure (maybe 0 (const 1) (_layoutManager_shouldBreakLineByWordBeforeCharacterAtIndex rec_))
    else if queriedSel == sel_layoutManager_shouldBreakLineByHyphenatingBeforeCharacterAtIndex then pure (maybe 0 (const 1) (_layoutManager_shouldBreakLineByHyphenatingBeforeCharacterAtIndex rec_))
    else if queriedSel == sel_layoutManagerDidInvalidateLayout then pure (maybe 0 (const 1) (_layoutManagerDidInvalidateLayout rec_))
    else if queriedSel == sel_layoutManager_didCompleteLayoutForTextContainer_atEnd then pure (maybe 0 (const 1) (_layoutManager_didCompleteLayoutForTextContainer_atEnd rec_))
    else if queriedSel == sel_layoutManager_shouldUseTemporaryAttributes_forDrawingToScreen_atCharacterIndex_effectiveRange then pure (maybe 0 (const 1) (_layoutManager_shouldUseTemporaryAttributes_forDrawingToScreen_atCharacterIndex_effectiveRange rec_))
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
newNSLayoutManagerDelegate :: NSLayoutManagerDelegateOverrides -> IO RawId
newNSLayoutManagerDelegate overrides = do
  inst <- class_createInstance nsLayoutManagerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
