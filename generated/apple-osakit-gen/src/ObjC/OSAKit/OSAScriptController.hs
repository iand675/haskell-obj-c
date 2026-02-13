{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , script
  , setScript
  , language
  , setLanguage
  , scriptState
  , compiling
  , compileScriptSelector
  , compilingSelector
  , languageSelector
  , recordScriptSelector
  , resultViewSelector
  , runScriptSelector
  , scriptSelector
  , scriptStateSelector
  , scriptViewSelector
  , setLanguageSelector
  , setResultViewSelector
  , setScriptSelector
  , setScriptViewSelector
  , stopScriptSelector

  -- * Enum types
  , OSAScriptState(OSAScriptState)
  , pattern OSAScriptStopped
  , pattern OSAScriptRunning
  , pattern OSAScriptRecording

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OSAKit.Internal.Classes
import ObjC.OSAKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- compileScript:@
compileScript :: IsOSAScriptController osaScriptController => osaScriptController -> RawId -> IO ()
compileScript osaScriptController sender =
  sendMessage osaScriptController compileScriptSelector sender

-- | @- recordScript:@
recordScript :: IsOSAScriptController osaScriptController => osaScriptController -> RawId -> IO ()
recordScript osaScriptController sender =
  sendMessage osaScriptController recordScriptSelector sender

-- | @- runScript:@
runScript :: IsOSAScriptController osaScriptController => osaScriptController -> RawId -> IO ()
runScript osaScriptController sender =
  sendMessage osaScriptController runScriptSelector sender

-- | @- stopScript:@
stopScript :: IsOSAScriptController osaScriptController => osaScriptController -> RawId -> IO ()
stopScript osaScriptController sender =
  sendMessage osaScriptController stopScriptSelector sender

-- | @- scriptView@
scriptView :: IsOSAScriptController osaScriptController => osaScriptController -> IO (Id OSAScriptView)
scriptView osaScriptController =
  sendMessage osaScriptController scriptViewSelector

-- | @- setScriptView:@
setScriptView :: (IsOSAScriptController osaScriptController, IsOSAScriptView value) => osaScriptController -> value -> IO ()
setScriptView osaScriptController value =
  sendMessage osaScriptController setScriptViewSelector (toOSAScriptView value)

-- | @- resultView@
resultView :: IsOSAScriptController osaScriptController => osaScriptController -> IO (Id NSTextView)
resultView osaScriptController =
  sendMessage osaScriptController resultViewSelector

-- | @- setResultView:@
setResultView :: (IsOSAScriptController osaScriptController, IsNSTextView value) => osaScriptController -> value -> IO ()
setResultView osaScriptController value =
  sendMessage osaScriptController setResultViewSelector (toNSTextView value)

-- | @- script@
script :: IsOSAScriptController osaScriptController => osaScriptController -> IO (Id OSAScript)
script osaScriptController =
  sendMessage osaScriptController scriptSelector

-- | @- setScript:@
setScript :: (IsOSAScriptController osaScriptController, IsOSAScript value) => osaScriptController -> value -> IO ()
setScript osaScriptController value =
  sendMessage osaScriptController setScriptSelector (toOSAScript value)

-- | @- language@
language :: IsOSAScriptController osaScriptController => osaScriptController -> IO (Id OSALanguage)
language osaScriptController =
  sendMessage osaScriptController languageSelector

-- | @- setLanguage:@
setLanguage :: (IsOSAScriptController osaScriptController, IsOSALanguage value) => osaScriptController -> value -> IO ()
setLanguage osaScriptController value =
  sendMessage osaScriptController setLanguageSelector (toOSALanguage value)

-- | @- scriptState@
scriptState :: IsOSAScriptController osaScriptController => osaScriptController -> IO OSAScriptState
scriptState osaScriptController =
  sendMessage osaScriptController scriptStateSelector

-- | @- compiling@
compiling :: IsOSAScriptController osaScriptController => osaScriptController -> IO Bool
compiling osaScriptController =
  sendMessage osaScriptController compilingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compileScript:@
compileScriptSelector :: Selector '[RawId] ()
compileScriptSelector = mkSelector "compileScript:"

-- | @Selector@ for @recordScript:@
recordScriptSelector :: Selector '[RawId] ()
recordScriptSelector = mkSelector "recordScript:"

-- | @Selector@ for @runScript:@
runScriptSelector :: Selector '[RawId] ()
runScriptSelector = mkSelector "runScript:"

-- | @Selector@ for @stopScript:@
stopScriptSelector :: Selector '[RawId] ()
stopScriptSelector = mkSelector "stopScript:"

-- | @Selector@ for @scriptView@
scriptViewSelector :: Selector '[] (Id OSAScriptView)
scriptViewSelector = mkSelector "scriptView"

-- | @Selector@ for @setScriptView:@
setScriptViewSelector :: Selector '[Id OSAScriptView] ()
setScriptViewSelector = mkSelector "setScriptView:"

-- | @Selector@ for @resultView@
resultViewSelector :: Selector '[] (Id NSTextView)
resultViewSelector = mkSelector "resultView"

-- | @Selector@ for @setResultView:@
setResultViewSelector :: Selector '[Id NSTextView] ()
setResultViewSelector = mkSelector "setResultView:"

-- | @Selector@ for @script@
scriptSelector :: Selector '[] (Id OSAScript)
scriptSelector = mkSelector "script"

-- | @Selector@ for @setScript:@
setScriptSelector :: Selector '[Id OSAScript] ()
setScriptSelector = mkSelector "setScript:"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id OSALanguage)
languageSelector = mkSelector "language"

-- | @Selector@ for @setLanguage:@
setLanguageSelector :: Selector '[Id OSALanguage] ()
setLanguageSelector = mkSelector "setLanguage:"

-- | @Selector@ for @scriptState@
scriptStateSelector :: Selector '[] OSAScriptState
scriptStateSelector = mkSelector "scriptState"

-- | @Selector@ for @compiling@
compilingSelector :: Selector '[] Bool
compilingSelector = mkSelector "compiling"

