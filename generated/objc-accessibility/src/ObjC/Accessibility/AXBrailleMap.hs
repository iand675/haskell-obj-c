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
  , presentImageSelector
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presentImage:@
presentImage :: IsAXBrailleMap axBrailleMap => axBrailleMap -> Ptr () -> IO ()
presentImage axBrailleMap  image =
  sendMsg axBrailleMap (mkSelector "presentImage:") retVoid [argPtr image]

-- | @- init@
init_ :: IsAXBrailleMap axBrailleMap => axBrailleMap -> IO (Id AXBrailleMap)
init_ axBrailleMap  =
  sendMsg axBrailleMap (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXBrailleMap)
new  =
  do
    cls' <- getRequiredClass "AXBrailleMap"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentImage:@
presentImageSelector :: Selector
presentImageSelector = mkSelector "presentImage:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

