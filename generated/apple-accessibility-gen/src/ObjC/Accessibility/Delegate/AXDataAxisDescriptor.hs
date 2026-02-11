{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AXDataAxisDescriptor@.
--
-- Usage:
--
-- @
-- delegate <- newAXDataAxisDescriptor defaultAXDataAxisDescriptorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Accessibility.Delegate.AXDataAxisDescriptor
  ( AXDataAxisDescriptorOverrides(..)
  , defaultAXDataAxisDescriptorOverrides
  , newAXDataAxisDescriptor
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

-- | Overrides record for @\@protocol AXDataAxisDescriptor@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AXDataAxisDescriptorOverrides = AXDataAxisDescriptorOverrides
  { _title :: !(Maybe (IO RawId))
  , _setTitle :: !(Maybe (RawId -> IO ()))
  , _attributedTitle :: !(Maybe (IO RawId))
  , _setAttributedTitle :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAXDataAxisDescriptorOverrides :: AXDataAxisDescriptorOverrides
defaultAXDataAxisDescriptorOverrides = AXDataAxisDescriptorOverrides
  { _title = Nothing
  , _setTitle = Nothing
  , _attributedTitle = Nothing
  , _setAttributedTitle = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE axDataAxisDescriptorDelegateClass #-}
axDataAxisDescriptorDelegateClass :: Class
axDataAxisDescriptorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAXDataAxisDescriptor" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_title = unSelector (mkSelector "title")
      sel_setTitle = unSelector (mkSelector "setTitle:")
      sel_attributedTitle = unSelector (mkSelector "attributedTitle")
      sel_setAttributedTitle = unSelector (mkSelector "setAttributedTitle:")
  -- title
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AXDataAxisDescriptorOverrides
    case _title rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "title" "@@:" stub_0

  -- setTitle:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AXDataAxisDescriptorOverrides
    case _setTitle rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setTitle:" "v@:@" stub_1

  -- attributedTitle
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AXDataAxisDescriptorOverrides
    case _attributedTitle rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "attributedTitle" "@@:" stub_2

  -- setAttributedTitle:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AXDataAxisDescriptorOverrides
    case _setAttributedTitle rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAttributedTitle:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AXDataAxisDescriptorOverrides
    if queriedSel == sel_title then pure (maybe 0 (const 1) (_title rec_))
    else if queriedSel == sel_setTitle then pure (maybe 0 (const 1) (_setTitle rec_))
    else if queriedSel == sel_attributedTitle then pure (maybe 0 (const 1) (_attributedTitle rec_))
    else if queriedSel == sel_setAttributedTitle then pure (maybe 0 (const 1) (_setAttributedTitle rec_))
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
newAXDataAxisDescriptor :: AXDataAxisDescriptorOverrides -> IO RawId
newAXDataAxisDescriptor overrides = do
  inst <- class_createInstance axDataAxisDescriptorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
