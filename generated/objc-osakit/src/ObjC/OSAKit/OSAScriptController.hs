{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSAScriptController@.
module ObjC.OSAKit.OSAScriptController
  ( OSAScriptController
  , IsOSAScriptController(..)
  , compileScript
  , recordScript
  , runScript
  , stopScript
  , scriptView
  , setScriptView
  , resultView
  , setResultView
  , language
  , setLanguage
  , scriptState
  , compiling
  , compileScriptSelector
  , recordScriptSelector
  , runScriptSelector
  , stopScriptSelector
  , scriptViewSelector
  , setScriptViewSelector
  , resultViewSelector
  , setResultViewSelector
  , languageSelector
  , setLanguageSelector
  , scriptStateSelector
  , compilingSelector

  -- * Enum types
  , OSAScriptState(OSAScriptState)
  , pattern OSAScriptStopped
  , pattern OSAScriptRunning
  , pattern OSAScriptRecording

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OSAKit.Internal.Classes
import ObjC.OSAKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- compileScript:@
compileScript :: IsOSAScriptController osaScriptController => osaScriptController -> RawId -> IO ()
compileScript osaScriptController  sender =
  sendMsg osaScriptController (mkSelector "compileScript:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- recordScript:@
recordScript :: IsOSAScriptController osaScriptController => osaScriptController -> RawId -> IO ()
recordScript osaScriptController  sender =
  sendMsg osaScriptController (mkSelector "recordScript:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- runScript:@
runScript :: IsOSAScriptController osaScriptController => osaScriptController -> RawId -> IO ()
runScript osaScriptController  sender =
  sendMsg osaScriptController (mkSelector "runScript:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stopScript:@
stopScript :: IsOSAScriptController osaScriptController => osaScriptController -> RawId -> IO ()
stopScript osaScriptController  sender =
  sendMsg osaScriptController (mkSelector "stopScript:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- scriptView@
scriptView :: IsOSAScriptController osaScriptController => osaScriptController -> IO (Id OSAScriptView)
scriptView osaScriptController  =
  sendMsg osaScriptController (mkSelector "scriptView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScriptView:@
setScriptView :: (IsOSAScriptController osaScriptController, IsOSAScriptView value) => osaScriptController -> value -> IO ()
setScriptView osaScriptController  value =
withObjCPtr value $ \raw_value ->
    sendMsg osaScriptController (mkSelector "setScriptView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resultView@
resultView :: IsOSAScriptController osaScriptController => osaScriptController -> IO (Id NSTextView)
resultView osaScriptController  =
  sendMsg osaScriptController (mkSelector "resultView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResultView:@
setResultView :: (IsOSAScriptController osaScriptController, IsNSTextView value) => osaScriptController -> value -> IO ()
setResultView osaScriptController  value =
withObjCPtr value $ \raw_value ->
    sendMsg osaScriptController (mkSelector "setResultView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- language@
language :: IsOSAScriptController osaScriptController => osaScriptController -> IO (Id OSALanguage)
language osaScriptController  =
  sendMsg osaScriptController (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLanguage:@
setLanguage :: (IsOSAScriptController osaScriptController, IsOSALanguage value) => osaScriptController -> value -> IO ()
setLanguage osaScriptController  value =
withObjCPtr value $ \raw_value ->
    sendMsg osaScriptController (mkSelector "setLanguage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- scriptState@
scriptState :: IsOSAScriptController osaScriptController => osaScriptController -> IO OSAScriptState
scriptState osaScriptController  =
  fmap (coerce :: CLong -> OSAScriptState) $ sendMsg osaScriptController (mkSelector "scriptState") retCLong []

-- | @- compiling@
compiling :: IsOSAScriptController osaScriptController => osaScriptController -> IO Bool
compiling osaScriptController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaScriptController (mkSelector "compiling") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compileScript:@
compileScriptSelector :: Selector
compileScriptSelector = mkSelector "compileScript:"

-- | @Selector@ for @recordScript:@
recordScriptSelector :: Selector
recordScriptSelector = mkSelector "recordScript:"

-- | @Selector@ for @runScript:@
runScriptSelector :: Selector
runScriptSelector = mkSelector "runScript:"

-- | @Selector@ for @stopScript:@
stopScriptSelector :: Selector
stopScriptSelector = mkSelector "stopScript:"

-- | @Selector@ for @scriptView@
scriptViewSelector :: Selector
scriptViewSelector = mkSelector "scriptView"

-- | @Selector@ for @setScriptView:@
setScriptViewSelector :: Selector
setScriptViewSelector = mkSelector "setScriptView:"

-- | @Selector@ for @resultView@
resultViewSelector :: Selector
resultViewSelector = mkSelector "resultView"

-- | @Selector@ for @setResultView:@
setResultViewSelector :: Selector
setResultViewSelector = mkSelector "setResultView:"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @setLanguage:@
setLanguageSelector :: Selector
setLanguageSelector = mkSelector "setLanguage:"

-- | @Selector@ for @scriptState@
scriptStateSelector :: Selector
scriptStateSelector = mkSelector "scriptState"

-- | @Selector@ for @compiling@
compilingSelector :: Selector
compilingSelector = mkSelector "compiling"

