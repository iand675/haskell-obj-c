{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PHProjectExtensionController@.
--
-- Usage:
--
-- @
-- delegate <- newPHProjectExtensionController defaultPHProjectExtensionControllerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PhotosUI.Delegate.PHProjectExtensionController
  ( PHProjectExtensionControllerOverrides(..)
  , defaultPHProjectExtensionControllerOverrides
  , newPHProjectExtensionController
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

-- | Overrides record for @\@protocol PHProjectExtensionController@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PHProjectExtensionControllerOverrides = PHProjectExtensionControllerOverrides
  { _typeDescriptionDataSourceForCategory_invalidator :: !(Maybe (RawId -> RawId -> IO RawId))
  , _supportedProjectTypes :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultPHProjectExtensionControllerOverrides :: PHProjectExtensionControllerOverrides
defaultPHProjectExtensionControllerOverrides = PHProjectExtensionControllerOverrides
  { _typeDescriptionDataSourceForCategory_invalidator = Nothing
  , _supportedProjectTypes = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE phProjectExtensionControllerDelegateClass #-}
phProjectExtensionControllerDelegateClass :: Class
phProjectExtensionControllerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPHProjectExtensionController" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_typeDescriptionDataSourceForCategory_invalidator = unSelector (mkSelector "typeDescriptionDataSourceForCategory:invalidator:")
      sel_supportedProjectTypes = unSelector (mkSelector "supportedProjectTypes")
  -- typeDescriptionDataSourceForCategory:invalidator:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectExtensionControllerOverrides
    case _typeDescriptionDataSourceForCategory_invalidator rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "typeDescriptionDataSourceForCategory:invalidator:" "@@:@@" stub_0

  -- supportedProjectTypes
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectExtensionControllerOverrides
    case _supportedProjectTypes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedProjectTypes" "@@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectExtensionControllerOverrides
    if queriedSel == sel_typeDescriptionDataSourceForCategory_invalidator then pure (maybe 0 (const 1) (_typeDescriptionDataSourceForCategory_invalidator rec_))
    else if queriedSel == sel_supportedProjectTypes then pure (maybe 0 (const 1) (_supportedProjectTypes rec_))
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
newPHProjectExtensionController :: PHProjectExtensionControllerOverrides -> IO RawId
newPHProjectExtensionController overrides = do
  inst <- class_createInstance phProjectExtensionControllerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
