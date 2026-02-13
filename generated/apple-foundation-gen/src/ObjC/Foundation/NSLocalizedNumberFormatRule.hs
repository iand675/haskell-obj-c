{-# LANGUAGE DataKinds #-}
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
  , automaticSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "NSLocalizedNumberFormatRule"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSLocalizedNumberFormatRule nsLocalizedNumberFormatRule => nsLocalizedNumberFormatRule -> IO RawId
init_ nsLocalizedNumberFormatRule =
  sendOwnedMessage nsLocalizedNumberFormatRule initSelector

-- | @+ automatic@
automatic :: IO (Id NSLocalizedNumberFormatRule)
automatic  =
  do
    cls' <- getRequiredClass "NSLocalizedNumberFormatRule"
    sendClassMessage cls' automaticSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @automatic@
automaticSelector :: Selector '[] (Id NSLocalizedNumberFormatRule)
automaticSelector = mkSelector "automatic"

