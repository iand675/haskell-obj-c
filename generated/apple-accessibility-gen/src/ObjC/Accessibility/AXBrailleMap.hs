{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXBrailleMap@.
module ObjC.Accessibility.AXBrailleMap
  ( AXBrailleMap
  , IsAXBrailleMap(..)
  , presentImage
  , init_
  , new
  , initSelector
  , newSelector
  , presentImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presentImage:@
presentImage :: IsAXBrailleMap axBrailleMap => axBrailleMap -> Ptr () -> IO ()
presentImage axBrailleMap image =
  sendMessage axBrailleMap presentImageSelector image

-- | @- init@
init_ :: IsAXBrailleMap axBrailleMap => axBrailleMap -> IO (Id AXBrailleMap)
init_ axBrailleMap =
  sendOwnedMessage axBrailleMap initSelector

-- | @+ new@
new :: IO (Id AXBrailleMap)
new  =
  do
    cls' <- getRequiredClass "AXBrailleMap"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentImage:@
presentImageSelector :: Selector '[Ptr ()] ()
presentImageSelector = mkSelector "presentImage:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXBrailleMap)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXBrailleMap)
newSelector = mkSelector "new"

