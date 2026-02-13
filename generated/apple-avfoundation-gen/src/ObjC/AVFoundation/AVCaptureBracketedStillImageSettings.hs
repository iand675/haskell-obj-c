{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureBracketedStillImageSettings
--
-- AVCaptureBracketedStillImageSettings is an abstract base class that defines an interface for settings pertaining to a bracketed capture.
--
-- AVCaptureBracketedStillImageSettings may not be instantiated directly.
--
-- Generated bindings for @AVCaptureBracketedStillImageSettings@.
module ObjC.AVFoundation.AVCaptureBracketedStillImageSettings
  ( AVCaptureBracketedStillImageSettings
  , IsAVCaptureBracketedStillImageSettings(..)
  , init_
  , new
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureBracketedStillImageSettings avCaptureBracketedStillImageSettings => avCaptureBracketedStillImageSettings -> IO (Id AVCaptureBracketedStillImageSettings)
init_ avCaptureBracketedStillImageSettings =
  sendOwnedMessage avCaptureBracketedStillImageSettings initSelector

-- | @+ new@
new :: IO (Id AVCaptureBracketedStillImageSettings)
new  =
  do
    cls' <- getRequiredClass "AVCaptureBracketedStillImageSettings"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureBracketedStillImageSettings)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureBracketedStillImageSettings)
newSelector = mkSelector "new"

