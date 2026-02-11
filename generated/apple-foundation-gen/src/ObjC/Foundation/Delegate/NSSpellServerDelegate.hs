{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSSpellServerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSSpellServerDelegate defaultNSSpellServerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSSpellServerDelegate
  ( NSSpellServerDelegateOverrides(..)
  , defaultNSSpellServerDelegateOverrides
  , newNSSpellServerDelegate
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

-- | Overrides record for @\@protocol NSSpellServerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSSpellServerDelegateOverrides = NSSpellServerDelegateOverrides
  { _spellServer_suggestGuessesForWord_inLanguage :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _spellServer_didLearnWord_inLanguage :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _spellServer_didForgetWord_inLanguage :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _spellServer_checkString_offset_types_options_orthography_wordCount :: !(Maybe (RawId -> RawId -> Int -> Int -> RawId -> RawId -> RawId -> IO RawId))
  , _spellServer_recordResponse_toCorrection_forWord_language :: !(Maybe (RawId -> Int -> RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSSpellServerDelegateOverrides :: NSSpellServerDelegateOverrides
defaultNSSpellServerDelegateOverrides = NSSpellServerDelegateOverrides
  { _spellServer_suggestGuessesForWord_inLanguage = Nothing
  , _spellServer_didLearnWord_inLanguage = Nothing
  , _spellServer_didForgetWord_inLanguage = Nothing
  , _spellServer_checkString_offset_types_options_orthography_wordCount = Nothing
  , _spellServer_recordResponse_toCorrection_forWord_language = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_Q_Q_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsSpellServerDelegateDelegateClass #-}
nsSpellServerDelegateDelegateClass :: Class
nsSpellServerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSSpellServerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_spellServer_suggestGuessesForWord_inLanguage = unSelector (mkSelector "spellServer:suggestGuessesForWord:inLanguage:")
      sel_spellServer_didLearnWord_inLanguage = unSelector (mkSelector "spellServer:didLearnWord:inLanguage:")
      sel_spellServer_didForgetWord_inLanguage = unSelector (mkSelector "spellServer:didForgetWord:inLanguage:")
      sel_spellServer_checkString_offset_types_options_orthography_wordCount = unSelector (mkSelector "spellServer:checkString:offset:types:options:orthography:wordCount:")
      sel_spellServer_recordResponse_toCorrection_forWord_language = unSelector (mkSelector "spellServer:recordResponse:toCorrection:forWord:language:")
  -- spellServer:suggestGuessesForWord:inLanguage:
  stub_0 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpellServerDelegateOverrides
    case _spellServer_suggestGuessesForWord_inLanguage rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "spellServer:suggestGuessesForWord:inLanguage:" "@@:@@@" stub_0

  -- spellServer:didLearnWord:inLanguage:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpellServerDelegateOverrides
    case _spellServer_didLearnWord_inLanguage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "spellServer:didLearnWord:inLanguage:" "v@:@@@" stub_1

  -- spellServer:didForgetWord:inLanguage:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpellServerDelegateOverrides
    case _spellServer_didForgetWord_inLanguage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "spellServer:didForgetWord:inLanguage:" "v@:@@@" stub_2

  -- spellServer:checkString:offset:types:options:orthography:wordCount:
  stub_3 <- wrap_at_at_Q_Q_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 arg4 arg5 arg6 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpellServerDelegateOverrides
    case _spellServer_checkString_offset_types_options_orthography_wordCount rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2) (fromIntegral arg3) (RawId arg4) (RawId arg5) (RawId arg6)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "spellServer:checkString:offset:types:options:orthography:wordCount:" "@@:@@QQ@@@" stub_3

  -- spellServer:recordResponse:toCorrection:forWord:language:
  stub_4 <- wrap_at_Q_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpellServerDelegateOverrides
    case _spellServer_recordResponse_toCorrection_forWord_language rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (RawId arg2) (RawId arg3) (RawId arg4)
  addObjCMethod cls "spellServer:recordResponse:toCorrection:forWord:language:" "v@:@Q@@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpellServerDelegateOverrides
    if queriedSel == sel_spellServer_suggestGuessesForWord_inLanguage then pure (maybe 0 (const 1) (_spellServer_suggestGuessesForWord_inLanguage rec_))
    else if queriedSel == sel_spellServer_didLearnWord_inLanguage then pure (maybe 0 (const 1) (_spellServer_didLearnWord_inLanguage rec_))
    else if queriedSel == sel_spellServer_didForgetWord_inLanguage then pure (maybe 0 (const 1) (_spellServer_didForgetWord_inLanguage rec_))
    else if queriedSel == sel_spellServer_checkString_offset_types_options_orthography_wordCount then pure (maybe 0 (const 1) (_spellServer_checkString_offset_types_options_orthography_wordCount rec_))
    else if queriedSel == sel_spellServer_recordResponse_toCorrection_forWord_language then pure (maybe 0 (const 1) (_spellServer_recordResponse_toCorrection_forWord_language rec_))
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
newNSSpellServerDelegate :: NSSpellServerDelegateOverrides -> IO RawId
newNSSpellServerDelegate overrides = do
  inst <- class_createInstance nsSpellServerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
