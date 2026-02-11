{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLocalizedNumberFormatRule@.
module ObjC.Foundation.NSLocalizedNumberFormatRule
  ( NSLocalizedNumberFormatRule
  , IsNSLocalizedNumberFormatRule(..)
  , new
  , init_
  , automatic
  , newSelector
  , initSelector
  , automaticSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "NSLocalizedNumberFormatRule"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "new") (retPtr retVoid) []

-- | @- init@
init_ :: IsNSLocalizedNumberFormatRule nsLocalizedNumberFormatRule => nsLocalizedNumberFormatRule -> IO RawId
init_ nsLocalizedNumberFormatRule  =
  fmap (RawId . castPtr) $ sendMsg nsLocalizedNumberFormatRule (mkSelector "init") (retPtr retVoid) []

-- | @+ automatic@
automatic :: IO (Id NSLocalizedNumberFormatRule)
automatic  =
  do
    cls' <- getRequiredClass "NSLocalizedNumberFormatRule"
    sendClassMsg cls' (mkSelector "automatic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @automatic@
automaticSelector :: Selector
automaticSelector = mkSelector "automatic"

