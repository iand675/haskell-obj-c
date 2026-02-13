{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BETextAlternatives@.
module ObjC.BrowserEngineKit.BETextAlternatives
  ( BETextAlternatives
  , IsBETextAlternatives(..)
  , new
  , init_
  , primaryString
  , alternativeStrings
  , alternativeStringsSelector
  , initSelector
  , newSelector
  , primaryStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- new@
new :: IsBETextAlternatives beTextAlternatives => beTextAlternatives -> IO (Id BETextAlternatives)
new beTextAlternatives =
  sendOwnedMessage beTextAlternatives newSelector

-- | @- init@
init_ :: IsBETextAlternatives beTextAlternatives => beTextAlternatives -> IO (Id BETextAlternatives)
init_ beTextAlternatives =
  sendOwnedMessage beTextAlternatives initSelector

-- | Original text for which alternative strings are provided
--
-- ObjC selector: @- primaryString@
primaryString :: IsBETextAlternatives beTextAlternatives => beTextAlternatives -> IO (Id NSString)
primaryString beTextAlternatives =
  sendMessage beTextAlternatives primaryStringSelector

-- | Array of available aternative strings
--
-- ObjC selector: @- alternativeStrings@
alternativeStrings :: IsBETextAlternatives beTextAlternatives => beTextAlternatives -> IO (Id NSArray)
alternativeStrings beTextAlternatives =
  sendMessage beTextAlternatives alternativeStringsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BETextAlternatives)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BETextAlternatives)
initSelector = mkSelector "init"

-- | @Selector@ for @primaryString@
primaryStringSelector :: Selector '[] (Id NSString)
primaryStringSelector = mkSelector "primaryString"

-- | @Selector@ for @alternativeStrings@
alternativeStringsSelector :: Selector '[] (Id NSArray)
alternativeStringsSelector = mkSelector "alternativeStrings"

