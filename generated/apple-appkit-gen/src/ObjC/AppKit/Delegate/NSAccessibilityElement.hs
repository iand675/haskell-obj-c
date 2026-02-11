{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilityElement@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilityElement defaultNSAccessibilityElementOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilityElement
  ( NSAccessibilityElementOverrides(..)
  , defaultNSAccessibilityElementOverrides
  , newNSAccessibilityElement
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

-- | Overrides record for @\@protocol NSAccessibilityElement@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityElementOverrides = NSAccessibilityElementOverrides
  { _accessibilityParent :: !(Maybe (IO RawId))
  , _isAccessibilityFocused :: !(Maybe (IO Bool))
  , _accessibilityIdentifier :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityElementOverrides :: NSAccessibilityElementOverrides
defaultNSAccessibilityElementOverrides = NSAccessibilityElementOverrides
  { _accessibilityParent = Nothing
  , _isAccessibilityFocused = Nothing
  , _accessibilityIdentifier = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAccessibilityElementDelegateClass #-}
nsAccessibilityElementDelegateClass :: Class
nsAccessibilityElementDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilityElement" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityParent = unSelector (mkSelector "accessibilityParent")
      sel_isAccessibilityFocused = unSelector (mkSelector "isAccessibilityFocused")
      sel_accessibilityIdentifier = unSelector (mkSelector "accessibilityIdentifier")
  -- accessibilityParent
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityElementOverrides
    case _accessibilityParent rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityParent" "@@:" stub_0

  -- isAccessibilityFocused
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityElementOverrides
    case _isAccessibilityFocused rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "isAccessibilityFocused" "B@:" stub_1

  -- accessibilityIdentifier
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityElementOverrides
    case _accessibilityIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityIdentifier" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityElementOverrides
    if queriedSel == sel_accessibilityParent then pure (maybe 0 (const 1) (_accessibilityParent rec_))
    else if queriedSel == sel_isAccessibilityFocused then pure (maybe 0 (const 1) (_isAccessibilityFocused rec_))
    else if queriedSel == sel_accessibilityIdentifier then pure (maybe 0 (const 1) (_accessibilityIdentifier rec_))
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
newNSAccessibilityElement :: NSAccessibilityElementOverrides -> IO RawId
newNSAccessibilityElement overrides = do
  inst <- class_createInstance nsAccessibilityElementDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
