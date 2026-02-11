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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureBracketedStillImageSettings avCaptureBracketedStillImageSettings => avCaptureBracketedStillImageSettings -> IO (Id AVCaptureBracketedStillImageSettings)
init_ avCaptureBracketedStillImageSettings  =
  sendMsg avCaptureBracketedStillImageSettings (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureBracketedStillImageSettings)
new  =
  do
    cls' <- getRequiredClass "AVCaptureBracketedStillImageSettings"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

