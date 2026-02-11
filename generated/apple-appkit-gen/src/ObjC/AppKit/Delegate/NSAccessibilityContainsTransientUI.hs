{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilityContainsTransientUI@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilityContainsTransientUI defaultNSAccessibilityContainsTransientUIOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilityContainsTransientUI
  ( NSAccessibilityContainsTransientUIOverrides(..)
  , defaultNSAccessibilityContainsTransientUIOverrides
  , newNSAccessibilityContainsTransientUI
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

-- | Overrides record for @\@protocol NSAccessibilityContainsTransientUI@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityContainsTransientUIOverrides = NSAccessibilityContainsTransientUIOverrides
  { _accessibilityPerformShowAlternateUI :: !(Maybe (IO Bool))
  , _accessibilityPerformShowDefaultUI :: !(Maybe (IO Bool))
  , _isAccessibilityAlternateUIVisible :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityContainsTransientUIOverrides :: NSAccessibilityContainsTransientUIOverrides
defaultNSAccessibilityContainsTransientUIOverrides = NSAccessibilityContainsTransientUIOverrides
  { _accessibilityPerformShowAlternateUI = Nothing
  , _accessibilityPerformShowDefaultUI = Nothing
  , _isAccessibilityAlternateUIVisible = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAccessibilityContainsTransientUIDelegateClass #-}
nsAccessibilityContainsTransientUIDelegateClass :: Class
nsAccessibilityContainsTransientUIDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilityContainsTransientUI" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityPerformShowAlternateUI = unSelector (mkSelector "accessibilityPerformShowAlternateUI")
      sel_accessibilityPerformShowDefaultUI = unSelector (mkSelector "accessibilityPerformShowDefaultUI")
      sel_isAccessibilityAlternateUIVisible = unSelector (mkSelector "isAccessibilityAlternateUIVisible")
  -- accessibilityPerformShowAlternateUI
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityContainsTransientUIOverrides
    case _accessibilityPerformShowAlternateUI rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformShowAlternateUI" "B@:" stub_0

  -- accessibilityPerformShowDefaultUI
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityContainsTransientUIOverrides
    case _accessibilityPerformShowDefaultUI rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformShowDefaultUI" "B@:" stub_1

  -- isAccessibilityAlternateUIVisible
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityContainsTransientUIOverrides
    case _isAccessibilityAlternateUIVisible rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "isAccessibilityAlternateUIVisible" "B@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityContainsTransientUIOverrides
    if queriedSel == sel_accessibilityPerformShowAlternateUI then pure (maybe 0 (const 1) (_accessibilityPerformShowAlternateUI rec_))
    else if queriedSel == sel_accessibilityPerformShowDefaultUI then pure (maybe 0 (const 1) (_accessibilityPerformShowDefaultUI rec_))
    else if queriedSel == sel_isAccessibilityAlternateUIVisible then pure (maybe 0 (const 1) (_isAccessibilityAlternateUIVisible rec_))
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
newNSAccessibilityContainsTransientUI :: NSAccessibilityContainsTransientUIOverrides -> IO RawId
newNSAccessibilityContainsTransientUI overrides = do
  inst <- class_createInstance nsAccessibilityContainsTransientUIDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
