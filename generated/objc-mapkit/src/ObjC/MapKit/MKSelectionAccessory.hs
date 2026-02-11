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
  , newSelector
  , initSelector
  , mapItemDetailWithPresentationStyleSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MKSelectionAccessory)
new  =
  do
    cls' <- getRequiredClass "MKSelectionAccessory"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMKSelectionAccessory mkSelectionAccessory => mkSelectionAccessory -> IO (Id MKSelectionAccessory)
init_ mkSelectionAccessory  =
  sendMsg mkSelectionAccessory (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ mapItemDetailWithPresentationStyle:@
mapItemDetailWithPresentationStyle :: IsMKMapItemDetailSelectionAccessoryPresentationStyle presentationStyle => presentationStyle -> IO (Id MKSelectionAccessory)
mapItemDetailWithPresentationStyle presentationStyle =
  do
    cls' <- getRequiredClass "MKSelectionAccessory"
    withObjCPtr presentationStyle $ \raw_presentationStyle ->
      sendClassMsg cls' (mkSelector "mapItemDetailWithPresentationStyle:") (retPtr retVoid) [argPtr (castPtr raw_presentationStyle :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @mapItemDetailWithPresentationStyle:@
mapItemDetailWithPresentationStyleSelector :: Selector
mapItemDetailWithPresentationStyleSelector = mkSelector "mapItemDetailWithPresentationStyle:"

