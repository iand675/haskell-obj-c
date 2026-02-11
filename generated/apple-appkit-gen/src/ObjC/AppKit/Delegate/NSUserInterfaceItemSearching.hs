{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSUserInterfaceItemSearching@.
--
-- Usage:
--
-- @
-- delegate <- newNSUserInterfaceItemSearching defaultNSUserInterfaceItemSearchingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSUserInterfaceItemSearching
  ( NSUserInterfaceItemSearchingOverrides(..)
  , defaultNSUserInterfaceItemSearchingOverrides
  , newNSUserInterfaceItemSearching
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

-- | Overrides record for @\@protocol NSUserInterfaceItemSearching@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSUserInterfaceItemSearchingOverrides = NSUserInterfaceItemSearchingOverrides
  { _localizedTitlesForItem :: !(Maybe (RawId -> IO RawId))
  , _performActionForItem :: !(Maybe (RawId -> IO ()))
  , _showAllHelpTopicsForSearchString :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSUserInterfaceItemSearchingOverrides :: NSUserInterfaceItemSearchingOverrides
defaultNSUserInterfaceItemSearchingOverrides = NSUserInterfaceItemSearchingOverrides
  { _localizedTitlesForItem = Nothing
  , _performActionForItem = Nothing
  , _showAllHelpTopicsForSearchString = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsUserInterfaceItemSearchingDelegateClass #-}
nsUserInterfaceItemSearchingDelegateClass :: Class
nsUserInterfaceItemSearchingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSUserInterfaceItemSearching" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_localizedTitlesForItem = unSelector (mkSelector "localizedTitlesForItem:")
      sel_performActionForItem = unSelector (mkSelector "performActionForItem:")
      sel_showAllHelpTopicsForSearchString = unSelector (mkSelector "showAllHelpTopicsForSearchString:")
  -- localizedTitlesForItem:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceItemSearchingOverrides
    case _localizedTitlesForItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "localizedTitlesForItem:" "@@:@" stub_0

  -- performActionForItem:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceItemSearchingOverrides
    case _performActionForItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "performActionForItem:" "v@:@" stub_1

  -- showAllHelpTopicsForSearchString:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceItemSearchingOverrides
    case _showAllHelpTopicsForSearchString rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "showAllHelpTopicsForSearchString:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceItemSearchingOverrides
    if queriedSel == sel_localizedTitlesForItem then pure (maybe 0 (const 1) (_localizedTitlesForItem rec_))
    else if queriedSel == sel_performActionForItem then pure (maybe 0 (const 1) (_performActionForItem rec_))
    else if queriedSel == sel_showAllHelpTopicsForSearchString then pure (maybe 0 (const 1) (_showAllHelpTopicsForSearchString rec_))
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
newNSUserInterfaceItemSearching :: NSUserInterfaceItemSearchingOverrides -> IO RawId
newNSUserInterfaceItemSearching overrides = do
  inst <- class_createInstance nsUserInterfaceItemSearchingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
