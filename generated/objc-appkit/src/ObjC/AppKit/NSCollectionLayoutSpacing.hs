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
  , flexibleSpacingSelector
  , fixedSpacingSelector
  , initSelector
  , newSelector
  , spacingSelector
  , isFlexibleSpacingSelector
  , isFixedSpacingSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ flexibleSpacing:@
flexibleSpacing :: CDouble -> IO (Id NSCollectionLayoutSpacing)
flexibleSpacing flexibleSpacing =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSpacing"
    sendClassMsg cls' (mkSelector "flexibleSpacing:") (retPtr retVoid) [argCDouble (fromIntegral flexibleSpacing)] >>= retainedObject . castPtr

-- | @+ fixedSpacing:@
fixedSpacing :: CDouble -> IO (Id NSCollectionLayoutSpacing)
fixedSpacing fixedSpacing =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSpacing"
    sendClassMsg cls' (mkSelector "fixedSpacing:") (retPtr retVoid) [argCDouble (fromIntegral fixedSpacing)] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutSpacing nsCollectionLayoutSpacing => nsCollectionLayoutSpacing -> IO (Id NSCollectionLayoutSpacing)
init_ nsCollectionLayoutSpacing  =
  sendMsg nsCollectionLayoutSpacing (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutSpacing)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSpacing"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- spacing@
spacing :: IsNSCollectionLayoutSpacing nsCollectionLayoutSpacing => nsCollectionLayoutSpacing -> IO CDouble
spacing nsCollectionLayoutSpacing  =
  sendMsg nsCollectionLayoutSpacing (mkSelector "spacing") retCDouble []

-- | @- isFlexibleSpacing@
isFlexibleSpacing :: IsNSCollectionLayoutSpacing nsCollectionLayoutSpacing => nsCollectionLayoutSpacing -> IO Bool
isFlexibleSpacing nsCollectionLayoutSpacing  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutSpacing (mkSelector "isFlexibleSpacing") retCULong []

-- | @- isFixedSpacing@
isFixedSpacing :: IsNSCollectionLayoutSpacing nsCollectionLayoutSpacing => nsCollectionLayoutSpacing -> IO Bool
isFixedSpacing nsCollectionLayoutSpacing  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutSpacing (mkSelector "isFixedSpacing") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @flexibleSpacing:@
flexibleSpacingSelector :: Selector
flexibleSpacingSelector = mkSelector "flexibleSpacing:"

-- | @Selector@ for @fixedSpacing:@
fixedSpacingSelector :: Selector
fixedSpacingSelector = mkSelector "fixedSpacing:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @spacing@
spacingSelector :: Selector
spacingSelector = mkSelector "spacing"

-- | @Selector@ for @isFlexibleSpacing@
isFlexibleSpacingSelector :: Selector
isFlexibleSpacingSelector = mkSelector "isFlexibleSpacing"

-- | @Selector@ for @isFixedSpacing@
isFixedSpacingSelector :: Selector
isFixedSpacingSelector = mkSelector "isFixedSpacing"

