{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilityLayoutArea@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilityLayoutArea defaultNSAccessibilityLayoutAreaOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilityLayoutArea
  ( NSAccessibilityLayoutAreaOverrides(..)
  , defaultNSAccessibilityLayoutAreaOverrides
  , newNSAccessibilityLayoutArea
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

-- | Overrides record for @\@protocol NSAccessibilityLayoutArea@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityLayoutAreaOverrides = NSAccessibilityLayoutAreaOverrides
  { _accessibilityLabel :: !(Maybe (IO RawId))
  , _accessibilityChildren :: !(Maybe (IO RawId))
  , _accessibilitySelectedChildren :: !(Maybe (IO RawId))
  , _accessibilityFocusedUIElement :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityLayoutAreaOverrides :: NSAccessibilityLayoutAreaOverrides
defaultNSAccessibilityLayoutAreaOverrides = NSAccessibilityLayoutAreaOverrides
  { _accessibilityLabel = Nothing
  , _accessibilityChildren = Nothing
  , _accessibilitySelectedChildren = Nothing
  , _accessibilityFocusedUIElement = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAccessibilityLayoutAreaDelegateClass #-}
nsAccessibilityLayoutAreaDelegateClass :: Class
nsAccessibilityLayoutAreaDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilityLayoutArea" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityLabel = unSelector (mkSelector "accessibilityLabel")
      sel_accessibilityChildren = unSelector (mkSelector "accessibilityChildren")
      sel_accessibilitySelectedChildren = unSelector (mkSelector "accessibilitySelectedChildren")
      sel_accessibilityFocusedUIElement = unSelector (mkSelector "accessibilityFocusedUIElement")
  -- accessibilityLabel
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityLayoutAreaOverrides
    case _accessibilityLabel rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityLabel" "@@:" stub_0

  -- accessibilityChildren
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityLayoutAreaOverrides
    case _accessibilityChildren rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityChildren" "@@:" stub_1

  -- accessibilitySelectedChildren
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityLayoutAreaOverrides
    case _accessibilitySelectedChildren rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedChildren" "@@:" stub_2

  -- accessibilityFocusedUIElement
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityLayoutAreaOverrides
    case _accessibilityFocusedUIElement rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityFocusedUIElement" "@@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityLayoutAreaOverrides
    if queriedSel == sel_accessibilityLabel then pure (maybe 0 (const 1) (_accessibilityLabel rec_))
    else if queriedSel == sel_accessibilityChildren then pure (maybe 0 (const 1) (_accessibilityChildren rec_))
    else if queriedSel == sel_accessibilitySelectedChildren then pure (maybe 0 (const 1) (_accessibilitySelectedChildren rec_))
    else if queriedSel == sel_accessibilityFocusedUIElement then pure (maybe 0 (const 1) (_accessibilityFocusedUIElement rec_))
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
newNSAccessibilityLayoutArea :: NSAccessibilityLayoutAreaOverrides -> IO RawId
newNSAccessibilityLayoutArea overrides = do
  inst <- class_createInstance nsAccessibilityLayoutAreaDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
