{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PKIdentityDocumentDescriptor@.
--
-- Usage:
--
-- @
-- delegate <- newPKIdentityDocumentDescriptor defaultPKIdentityDocumentDescriptorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PassKit.Delegate.PKIdentityDocumentDescriptor
  ( PKIdentityDocumentDescriptorOverrides(..)
  , defaultPKIdentityDocumentDescriptorOverrides
  , newPKIdentityDocumentDescriptor
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

-- | Overrides record for @\@protocol PKIdentityDocumentDescriptor@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PKIdentityDocumentDescriptorOverrides = PKIdentityDocumentDescriptorOverrides
  { _intentToStoreForElement :: !(Maybe (RawId -> IO RawId))
  , _addElements_withIntentToStore :: !(Maybe (RawId -> RawId -> IO ()))
  , _elements :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultPKIdentityDocumentDescriptorOverrides :: PKIdentityDocumentDescriptorOverrides
defaultPKIdentityDocumentDescriptorOverrides = PKIdentityDocumentDescriptorOverrides
  { _intentToStoreForElement = Nothing
  , _addElements_withIntentToStore = Nothing
  , _elements = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pkIdentityDocumentDescriptorDelegateClass #-}
pkIdentityDocumentDescriptorDelegateClass :: Class
pkIdentityDocumentDescriptorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPKIdentityDocumentDescriptor" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_intentToStoreForElement = unSelector (mkSelector "intentToStoreForElement:")
      sel_addElements_withIntentToStore = unSelector (mkSelector "addElements:withIntentToStore:")
      sel_elements = unSelector (mkSelector "elements")
  -- intentToStoreForElement:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKIdentityDocumentDescriptorOverrides
    case _intentToStoreForElement rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "intentToStoreForElement:" "@@:@" stub_0

  -- addElements:withIntentToStore:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKIdentityDocumentDescriptorOverrides
    case _addElements_withIntentToStore rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "addElements:withIntentToStore:" "v@:@@" stub_1

  -- elements
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKIdentityDocumentDescriptorOverrides
    case _elements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "elements" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKIdentityDocumentDescriptorOverrides
    if queriedSel == sel_intentToStoreForElement then pure (maybe 0 (const 1) (_intentToStoreForElement rec_))
    else if queriedSel == sel_addElements_withIntentToStore then pure (maybe 0 (const 1) (_addElements_withIntentToStore rec_))
    else if queriedSel == sel_elements then pure (maybe 0 (const 1) (_elements rec_))
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
newPKIdentityDocumentDescriptor :: PKIdentityDocumentDescriptorOverrides -> IO RawId
newPKIdentityDocumentDescriptor overrides = do
  inst <- class_createInstance pkIdentityDocumentDescriptorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
