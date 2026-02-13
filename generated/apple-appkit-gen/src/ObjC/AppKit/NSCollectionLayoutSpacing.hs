{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutSpacing@.
module ObjC.AppKit.NSCollectionLayoutSpacing
  ( NSCollectionLayoutSpacing
  , IsNSCollectionLayoutSpacing(..)
  , flexibleSpacing
  , fixedSpacing
  , init_
  , new
  , spacing
  , isFlexibleSpacing
  , isFixedSpacing
  , fixedSpacingSelector
  , flexibleSpacingSelector
  , initSelector
  , isFixedSpacingSelector
  , isFlexibleSpacingSelector
  , newSelector
  , spacingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ flexibleSpacing:@
flexibleSpacing :: CDouble -> IO (Id NSCollectionLayoutSpacing)
flexibleSpacing flexibleSpacing =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSpacing"
    sendClassMessage cls' flexibleSpacingSelector flexibleSpacing

-- | @+ fixedSpacing:@
fixedSpacing :: CDouble -> IO (Id NSCollectionLayoutSpacing)
fixedSpacing fixedSpacing =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSpacing"
    sendClassMessage cls' fixedSpacingSelector fixedSpacing

-- | @- init@
init_ :: IsNSCollectionLayoutSpacing nsCollectionLayoutSpacing => nsCollectionLayoutSpacing -> IO (Id NSCollectionLayoutSpacing)
init_ nsCollectionLayoutSpacing =
  sendOwnedMessage nsCollectionLayoutSpacing initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutSpacing)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSpacing"
    sendOwnedClassMessage cls' newSelector

-- | @- spacing@
spacing :: IsNSCollectionLayoutSpacing nsCollectionLayoutSpacing => nsCollectionLayoutSpacing -> IO CDouble
spacing nsCollectionLayoutSpacing =
  sendMessage nsCollectionLayoutSpacing spacingSelector

-- | @- isFlexibleSpacing@
isFlexibleSpacing :: IsNSCollectionLayoutSpacing nsCollectionLayoutSpacing => nsCollectionLayoutSpacing -> IO Bool
isFlexibleSpacing nsCollectionLayoutSpacing =
  sendMessage nsCollectionLayoutSpacing isFlexibleSpacingSelector

-- | @- isFixedSpacing@
isFixedSpacing :: IsNSCollectionLayoutSpacing nsCollectionLayoutSpacing => nsCollectionLayoutSpacing -> IO Bool
isFixedSpacing nsCollectionLayoutSpacing =
  sendMessage nsCollectionLayoutSpacing isFixedSpacingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @flexibleSpacing:@
flexibleSpacingSelector :: Selector '[CDouble] (Id NSCollectionLayoutSpacing)
flexibleSpacingSelector = mkSelector "flexibleSpacing:"

-- | @Selector@ for @fixedSpacing:@
fixedSpacingSelector :: Selector '[CDouble] (Id NSCollectionLayoutSpacing)
fixedSpacingSelector = mkSelector "fixedSpacing:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutSpacing)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutSpacing)
newSelector = mkSelector "new"

-- | @Selector@ for @spacing@
spacingSelector :: Selector '[] CDouble
spacingSelector = mkSelector "spacing"

-- | @Selector@ for @isFlexibleSpacing@
isFlexibleSpacingSelector :: Selector '[] Bool
isFlexibleSpacingSelector = mkSelector "isFlexibleSpacing"

-- | @Selector@ for @isFixedSpacing@
isFixedSpacingSelector :: Selector '[] Bool
isFixedSpacingSelector = mkSelector "isFixedSpacing"

