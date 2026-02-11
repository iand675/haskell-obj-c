{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AUCustomViewPersistentData@.
--
-- Usage:
--
-- @
-- delegate <- newAUCustomViewPersistentData defaultAUCustomViewPersistentDataOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreAudioKit.Delegate.AUCustomViewPersistentData
  ( AUCustomViewPersistentDataOverrides(..)
  , defaultAUCustomViewPersistentDataOverrides
  , newAUCustomViewPersistentData
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

-- | Overrides record for @\@protocol AUCustomViewPersistentData@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AUCustomViewPersistentDataOverrides = AUCustomViewPersistentDataOverrides
  { _customViewPersistentData :: !(Maybe (IO RawId))
  , _setCustomViewPersistentData :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAUCustomViewPersistentDataOverrides :: AUCustomViewPersistentDataOverrides
defaultAUCustomViewPersistentDataOverrides = AUCustomViewPersistentDataOverrides
  { _customViewPersistentData = Nothing
  , _setCustomViewPersistentData = Nothing
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
{-# NOINLINE auCustomViewPersistentDataDelegateClass #-}
auCustomViewPersistentDataDelegateClass :: Class
auCustomViewPersistentDataDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAUCustomViewPersistentData" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_customViewPersistentData = unSelector (mkSelector "customViewPersistentData")
      sel_setCustomViewPersistentData = unSelector (mkSelector "setCustomViewPersistentData:")
  -- customViewPersistentData
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AUCustomViewPersistentDataOverrides
    case _customViewPersistentData rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "customViewPersistentData" "@@:" stub_0

  -- setCustomViewPersistentData:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AUCustomViewPersistentDataOverrides
    case _setCustomViewPersistentData rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setCustomViewPersistentData:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AUCustomViewPersistentDataOverrides
    if queriedSel == sel_customViewPersistentData then pure (maybe 0 (const 1) (_customViewPersistentData rec_))
    else if queriedSel == sel_setCustomViewPersistentData then pure (maybe 0 (const 1) (_setCustomViewPersistentData rec_))
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
newAUCustomViewPersistentData :: AUCustomViewPersistentDataOverrides -> IO RawId
newAUCustomViewPersistentData overrides = do
  inst <- class_createInstance auCustomViewPersistentDataDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
