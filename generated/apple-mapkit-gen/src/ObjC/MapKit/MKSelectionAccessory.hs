{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKSelectionAccessory@.
module ObjC.MapKit.MKSelectionAccessory
  ( MKSelectionAccessory
  , IsMKSelectionAccessory(..)
  , new
  , init_
  , mapItemDetailWithPresentationStyle
  , initSelector
  , mapItemDetailWithPresentationStyleSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MKSelectionAccessory)
new  =
  do
    cls' <- getRequiredClass "MKSelectionAccessory"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMKSelectionAccessory mkSelectionAccessory => mkSelectionAccessory -> IO (Id MKSelectionAccessory)
init_ mkSelectionAccessory =
  sendOwnedMessage mkSelectionAccessory initSelector

-- | @+ mapItemDetailWithPresentationStyle:@
mapItemDetailWithPresentationStyle :: IsMKMapItemDetailSelectionAccessoryPresentationStyle presentationStyle => presentationStyle -> IO (Id MKSelectionAccessory)
mapItemDetailWithPresentationStyle presentationStyle =
  do
    cls' <- getRequiredClass "MKSelectionAccessory"
    sendClassMessage cls' mapItemDetailWithPresentationStyleSelector (toMKMapItemDetailSelectionAccessoryPresentationStyle presentationStyle)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKSelectionAccessory)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKSelectionAccessory)
initSelector = mkSelector "init"

-- | @Selector@ for @mapItemDetailWithPresentationStyle:@
mapItemDetailWithPresentationStyleSelector :: Selector '[Id MKMapItemDetailSelectionAccessoryPresentationStyle] (Id MKSelectionAccessory)
mapItemDetailWithPresentationStyleSelector = mkSelector "mapItemDetailWithPresentationStyle:"

