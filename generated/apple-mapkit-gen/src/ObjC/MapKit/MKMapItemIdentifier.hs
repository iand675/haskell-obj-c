{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapItemIdentifier@.
module ObjC.MapKit.MKMapItemIdentifier
  ( MKMapItemIdentifier
  , IsMKMapItemIdentifier(..)
  , init_
  , new
  , initWithIdentifierString
  , identifierString
  , identifierStringSelector
  , initSelector
  , initWithIdentifierStringSelector
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

-- | @- init@
init_ :: IsMKMapItemIdentifier mkMapItemIdentifier => mkMapItemIdentifier -> IO (Id MKMapItemIdentifier)
init_ mkMapItemIdentifier =
  sendOwnedMessage mkMapItemIdentifier initSelector

-- | @+ new@
new :: IO (Id MKMapItemIdentifier)
new  =
  do
    cls' <- getRequiredClass "MKMapItemIdentifier"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithIdentifierString:@
initWithIdentifierString :: (IsMKMapItemIdentifier mkMapItemIdentifier, IsNSString string) => mkMapItemIdentifier -> string -> IO (Id MKMapItemIdentifier)
initWithIdentifierString mkMapItemIdentifier string =
  sendOwnedMessage mkMapItemIdentifier initWithIdentifierStringSelector (toNSString string)

-- | @- identifierString@
identifierString :: IsMKMapItemIdentifier mkMapItemIdentifier => mkMapItemIdentifier -> IO (Id NSString)
identifierString mkMapItemIdentifier =
  sendMessage mkMapItemIdentifier identifierStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKMapItemIdentifier)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKMapItemIdentifier)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithIdentifierString:@
initWithIdentifierStringSelector :: Selector '[Id NSString] (Id MKMapItemIdentifier)
initWithIdentifierStringSelector = mkSelector "initWithIdentifierString:"

-- | @Selector@ for @identifierString@
identifierStringSelector :: Selector '[] (Id NSString)
identifierStringSelector = mkSelector "identifierString"

